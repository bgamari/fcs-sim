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
import Control.Monad.Primitive
import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Concurrent
import Control.Concurrent.Async
import GHC.Conc (getNumCapabilities)
import Options.Applicative

defaultBeamWidth = V3 400 400 1000  -- nm
defaultDiffusivity = 6.5^2 / 6 / 10  -- nm^2 / ns
defaultBoxSize = 15 *^ beamWidth

data Options = Opts { beamWidth     :: V3 Double
                    , diffusivity   :: Double
                    , boxSizeFactor :: Double
                    , timeStep      :: Double
                    , corrPts       :: Int
                    , minLag        :: Double
                    , maxLag        :: Double
                    }

-- Units:
--   length: nm
--   time;   ns

options :: Parser Options
options = Opts <$> option auto ( short 'w' <> long "beam-width" <> value defaultBeamWidth )
               <*> option auto ( short 'd' <> long "diffusivity" <> value defaultDiffusivity )
               <*> option auto ( short 'b' <> long "box-size-factor" <> value 15 )
               <*> option auto ( short 't' <> long "time-step" <> value 10 )
               <*> option auto ( short 'n' <> long "corr-pts" <> value 100 )
               <*> option auto ( short 'l' <> long "min-lag" <> value 1 )
               <*> option auto ( short 'L' <> long "max-lag" <> value 10000000 )

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n


sampleCorr :: BoxSize -> V3 Double -> Diffusivity -> Time -> VU.Vector Int
           -> Producer (VU.Vector (Time, Double)) IO ()
sampleCorr boxSize beamWidth diff dt taus = do
    a <- lift $ withSystemRandom $ asGenIO $ runRVarTWith id
         $ withTaus `fmap` correlateSample boxSize beamWidth sigma taus
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

concurrently :: Show a => Pipe a b IO r
             -> Producer a IO () -> Producer b IO ()
concurrently pipe prod = do
    nWorkers <- lift getNumCapabilities
    concurrently' nWorkers pipe prod

concurrently' :: Show a => Int -> Pipe a b IO r
              -> Producer a IO () -> Producer b IO ()
concurrently' nWorkers pipe prod = do
    (upOutput, upInput) <- lift $ spawn unbounded -- up-stream
    (downOutput, downInput) <- lift $ spawn unbounded -- down-stream
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

main = do
    args <- execParser $ info (helper <*> options) mempty
    let taus :: VU.Vector Int
        taus = VU.fromList $ map round $ logSpace (minLag args) (maxLag args) (corrPts args)
    let sampleCorr' = sampleCorr (beamWidth args ^* boxSizeFactor args) (beamWidth args) (diffusivity args) (timeStep args) taus
        sampleCorrs = Main.concurrently (asProducer $ const sampleCorr') (each $ replicate 10000 ())
    runEffect $ Pipes.for (PP.zip (each [1000..]) sampleCorrs) $ \(i,corr) -> do
        liftIO $ print i
        liftIO $ writeFile ("out-"++show i) $ printCorr corr

printCorr :: VU.Vector (Time, Double) -> String
printCorr = unlines . map (\(tau,g) -> printf "%1.5f\t%1.5f\n" tau g) . VU.toList

correlateSample :: (Monad m, PrimMonad m)
                => BoxSize -> V3 Double -> Length -> VU.Vector Int -> RVarT m (VU.Vector Double)
correlateSample boxSize beamWidth sigma taus = do
    traj <- evolveParticle boxSize sigma
    let intensity = V.map (beamIntensity beamWidth) traj
        norm = correlate 0 intensity
    return $ V.map (\tau->correlate tau intensity / norm) taus

correlate :: (V.Vector v a, RealFrac a) => Int -> v a -> a
correlate tau xs = V.sum $ V.zipWith (*) xs xs'
  where xs' = V.drop tau xs V.++ V.take tau xs
