{-# LANGUAGE FlexibleContexts #-}                

import FcsSim
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
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
import Control.Monad.Primitive.Class       
import Pipes
import Pipes.Concurrent
import Control.Concurrent.Async

beamWidth = V3 400 400 1000  -- nm
diff = 6.5^2 / 6 / 10  -- nm^2 / ns       
boxSize = 15 *^ beamWidth

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n

sampleCorrs :: Diffusivity -> Time -> VU.Vector Int
            -> Producer (VU.Vector (Time, Double)) IO ()
sampleCorrs diff dt taus =
   Main.concurrently 4 (asProducer $ const $ sampleCorr diff dt taus)
                       (each $ replicate 100 ())

sampleCorr :: Diffusivity -> Time -> VU.Vector Int
           -> Producer (VU.Vector (Time, Double)) IO ()
sampleCorr diff dt taus = do
    a <- lift $ withSystemRandom $ asGenIO $ runRVarTWith id
         $ withTaus `fmap` correlateSample sigma taus
    yield a
  where
    sigma = sqrt $ msd diff dt
    withTaus = V.zip (V.map (\t->realToFrac t * dt) taus)

asProducer :: Monad m => (a -> Producer b m r) -> Pipe a b m r
asProducer prod = forever $ await >>= go . prod
  where
    go :: Monad m => Producer b m r -> Pipe a b m ()
    go prod = do
      result <- lift $ next prod
      case result of
        Left r -> return ()
        Right (a, prod') -> yield a >> go prod'

concurrently :: Show a => Int -> Pipe a b IO r
             -> Producer a IO () -> Producer b IO ()
concurrently nWorkers pipe prod = do
    (upOutput, upInput) <- lift $ spawn Unbounded -- up-stream
    (downOutput, downInput) <- lift $ spawn Unbounded -- down-stream
    workers <- lift $ replicateM nWorkers $ async $ do
        runEffect $ fromInput upInput >-> void pipe >-> toOutput downOutput
        performGC
    let upWorker prod = do
            x <- next prod
            case x of
              Left r -> F.mapM_ wait workers
              Right (a, prod') -> do
                runEffect $ yield a >-> toOutput upOutput
                upWorker prod'
    lift $ async $ upWorker prod
    fromInput downInput

main = runEffect $ go >-> printPoint
  where
    printPoint = forever $ do
      corrs <- await
      V.forM_ corrs $ \(tau,corr)->
         lift $ printf "%1.5f\t%1.5f\n" tau corr

go :: Producer (VU.Vector (Time, Double)) IO ()
go = do
    sampleCorrs diff 10    taus
    sampleCorrs diff 100   taus
    sampleCorrs diff 1000  taus
    sampleCorrs diff 10000 taus
    sampleCorrs diff 100000 taus
  where
    taus = VU.fromList $ map round $ logSpace 1 100000 100

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
