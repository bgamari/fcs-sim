import FcsSim
import qualified Data.Vector.Unboxed as V
import Control.Monad.State
import Data.Random
import Data.Functor.Identity
import Data.Traversable as T
import Data.Foldable as F
import Linear
import Control.Monad
import System.Random.MWC hiding (uniform)
import Control.Applicative
import Text.Printf
import Statistics.Sample
import Control.Concurrent.ParallelIO

dt = 1e-3
beamWidth = 1   
n = round 1e5
taus = V.fromList $ map round
       $ logSpace 1 (0.8 * fromIntegral n) 200
nSamples = 100
diffusivities = replicate 20 1e1 ++ replicate 20 2e3

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n

main = do
    corrs <- parallel
             $ map (\d->withSystemRandom $ asGenIO $ runRVarTWith id
                        $ correlateSample d taus n
                   ) diffusivities
    let corrStats = V.fromList $ getZipList
                    $ fmap (meanVariance . V.fromList)
                    $ traverse (ZipList . V.toList) corrs
    V.forM_ (V.zip taus corrStats) $ \(tau,(mean,var))->
        printf "%1.2f\t%1.2f\t%1.2f\n" (fromIntegral tau::Double) mean (sqrt var)

takeSample :: Monad m => Diffusivity -> Int -> RVarT m (V.Vector Double)
takeSample d n = evalStateT (V.replicateM n $ evolveIntensity d dt beamWidth) (pure 0)

correlateSample :: Monad m
                => Diffusivity -> V.Vector Int -> Int -> RVarT m (V.Vector Double)
correlateSample d taus n = do
    samples <- takeSample d n
    return $ V.map (\tau->correlate tau samples) taus

correlate :: (V.Unbox a, RealFrac a) => Int -> V.Vector a -> a
correlate tau xs = V.sum $ V.zipWith (*) xs xs'
  where xs' = V.drop tau xs V.++ V.take tau xs
