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

n = round 1e5
taus = V.fromList $ map round
       $ logSpace 1 (0.8 * fromIntegral n) 200
nSamples = 100

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n

main = do
    corrs <- parallel $ replicate nSamples
             $ withSystemRandom $ asGenIO
             $ runRVarTWith id (correlateSample taus n)
    let corrStats = V.fromList $ getZipList
                    $ fmap (meanVariance . V.fromList)
                    $ traverse (ZipList . V.toList) corrs
    V.forM_ (V.zip taus corrStats) $ \(tau,(mean,var))->
        printf "%1.2f\t%1.2f\t%1.2f\n" (fromIntegral tau::Double) mean (sqrt var)

takeSample :: Monad m => Int -> RVarT m (V.Vector Double)
takeSample n = evalStateT (V.replicateM n evolveIntensity) (pure 0)

correlateSample :: Monad m => V.Vector Int -> Int -> RVarT m (V.Vector Double)
correlateSample taus n = do
    samples <- takeSample n
    return $ V.map (\tau->correlate tau samples) taus

correlate :: (V.Unbox a, RealFrac a) => Int -> V.Vector a -> a
correlate tau xs = V.sum $ V.zipWith (*) xs xs'
  where xs' = V.drop tau xs V.++ V.take tau xs