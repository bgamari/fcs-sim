import FcsSim
import qualified Data.Vector as V
import Control.Monad.State
import Data.Random
import Data.Functor.Identity
import Linear
import Control.Monad
import System.Random.MWC hiding (uniform)
import Control.Applicative

n = round 1e5
taus = V.fromList $ map round
       $ logSpace (log 1) (log $ fromIntegral n) 200

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n

main = withSystemRandom $ asGenIO $ \mwc->do
    corrs <- runRVarTWith id (replicateM 100 $ correlateSample taus n) mwc
    print corrs

takeSample :: Monad m => Int -> RVarT m (V.Vector Double)
takeSample n = evalStateT (V.replicateM n evolveIntensity) (pure 0)

correlateSample :: Monad m => V.Vector Int -> Int -> RVarT m (V.Vector Double)
correlateSample taus n = do
    samples <- takeSample n
    return $ V.map (\tau->correlate tau samples) taus

correlate :: RealFrac a => Int -> V.Vector a -> a
correlate tau xs = V.sum $ V.zipWith (*) xs xs'
  where xs' = V.drop tau xs V.++ V.take tau xs