{-# LANGUAGE FlexibleContexts #-}                

import FcsSim
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Control.Monad.State
import Control.Monad.Writer
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
import Control.Monad.Primitive.Class       

beamWidth = V3 400 400 1000  -- nm
diff = 6.5^2 / 6 / 10  -- nm^2 / ns       
boxSize = 15 *^ beamWidth

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n

sampleCorrs :: Diffusivity -> Time -> VU.Vector Int -> IO (VU.Vector (Time, Double))
sampleCorrs diff dt taus = do
   corrs <- parallel $ replicate 100 $ sampleCorr diff dt taus
   return $ V.concat corrs

sampleCorr :: Diffusivity -> Time -> VU.Vector Int -> IO (VU.Vector (Time, Double))
sampleCorr diff dt taus =
    withSystemRandom $ asGenIO $ runRVarTWith id
    $ withTaus `fmap` correlateSample sigma taus
  where
    sigma = sqrt $ msd diff dt
    withTaus = V.zip (V.map (\t->realToFrac t * dt) taus)

telling :: (Monoid a, Monad m) => m a -> WriterT a m ()
telling action = do
    x <- lift action
    x `seq` return ()
    tell x

main = do
    corrs <- go
    V.forM_ corrs $ \(tau,corr)->
         printf "%1.5f\t%1.5f\n" tau corr

go :: IO (VU.Vector (Time, Double))
go = execWriterT $ do
    telling $ sampleCorrs diff 10 (VU.fromList $ map round $ logSpace 1 10000 100)
    telling $ sampleCorrs diff 100 (VU.fromList $ map round $ logSpace 1 10000 100)
    telling $ sampleCorrs diff 1000 (VU.fromList $ map round $ logSpace 1 10000 100)

correlateSample :: (Monad m, MonadPrim (RVarT m))
                => Length -> VU.Vector Int -> RVarT m (VU.Vector Double)
correlateSample sigma taus = do
    traj <- evolveParticle boxSize sigma
    let intensity = V.map (beamIntensity beamWidth) traj
        norm = correlate 0 intensity
    return $ V.map (\tau->correlate tau intensity / norm) taus

correlate :: (V.Vector v a, RealFrac a) => Int -> v a -> a
correlate tau xs = V.sum $ V.zipWith (*) xs xs'
  where xs' = V.drop tau xs V.++ V.take tau xs
