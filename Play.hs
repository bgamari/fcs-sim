{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.Foldable
import Data.Semigroup ((<>))
import Data.List (nub)
import Data.Functor.Identity
import Linear
import Linear.Affine
import Numeric.Log
import Data.Random
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Streaming (Of, Stream)
import qualified System.Console.AsciiProgress as Progress

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU

import Control.Lens
import System.Random.MWC
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import GHC.Conc (getNumCapabilities, forkIO)
import Options.Applicative

import Reflection
import Propagator
import Types

-- | Gaussian beam intensity
beamIntensity :: BeamSize -> Point V3 Length -> Log Double
beamIntensity w (P x) = Exp (negate alpha / 2)
  where
    f wx xx = xx^2 / wx^2
    alpha = Data.Foldable.sum $ f <$> w <*> x
{-# INLINEABLE beamIntensity #-}

-- | Produce a random walk inside a simulation box until the path leaves
walkInsideBox :: PrimMonad m
              => BoxSize -> Length
              -> Stream (Of (Point V3 Length)) (RVarT m) ()
walkInsideBox boxSize sigma = do
    x0 <- lift $ pointInBox boxSize
    before <- lift $ streamToVector $ S.takeWhile (insideBox boxSize)
              $ propagateToStream (randomWalkP sigma) x0
    VU.mapM_ S.yield $ VU.reverse before
    S.takeWhile (insideBox boxSize) $ propagateToStream (randomWalkP sigma) x0

insideBox :: BoxSize -> Point V3 Length -> Bool
insideBox boxSize (P x) = Data.Foldable.and $ (\s x->abs x < (s/2)) <$> boxSize <*> x
{-# INLINEABLE insideBox #-}

instance PrimMonad m => PrimMonad (RVarT m) where
  type PrimState (RVarT m) = PrimState m
  primitive f = lift $ primitive f

stokesEinstein :: Length   -- ^ radius
               -> Viscosity
               -> Diffusivity
stokesEinstein r eta = boltzmann * 300 / 6 / pi / eta / r
  where boltzmann = 1.38e-23 -- kg * m^2 / s^2 / K

data Options = Opts { beamWidth     :: V3 Double
                    , diffusivity   :: Double
                    , boxSizeFactor :: Double
                    , timeStep      :: Double
                    , corrPts       :: Int
                    , minLag        :: Double
                    , maxLag        :: Double
                    }

options :: Parser Options
options = Opts <$> option auto ( short 'w' <> long "beam-width" <> value (V3 400 400 1000) <> help "size of excitation volume")
               <*> option auto ( short 'd' <> long "diffusivity" <> value 1.1e-3 <> help "diffusivity")
               <*> option auto ( short 'b' <> long "box-size-factor" <> value 20 <> help "size of simulation box")
               <*> option auto ( short 't' <> long "time-step" <> value 100 <> help "simulation timestep")
               <*> option auto ( short 'n' <> long "corr-pts" <> value 400 <> help "number of points to sample of correlation function")
               <*> option auto ( short 'l' <> long "min-lag" <> value 10000 <> help "minimum lag in nanoseconds")
               <*> option auto ( short 'L' <> long "max-lag" <> value 3e9 <> help "minimum lag in nanoseconds")

data Mode = ModeDroplet | ModeWalkInCube

decimate :: Monad m => Int -> Stream (Of a) m r -> Stream (Of a) m r
decimate n = S.catMaybes . S.mapped (S.head) . S.chunksOf n

takeWithProgress :: S.MonadIO m => Int -> Stream (Of a) m r -> Stream (Of a) m ()
takeWithProgress n s = do
    pg <- S.liftIO $ Progress.newProgressBar
        Progress.def { Progress.pgTotal = fromIntegral n
                     , Progress.pgOnCompletion = Just "Done :percent after :elapsed seconds"
                     }

    let step = 2^17
    let f :: S.MonadIO m => Int -> Stream (Of a) m r -> Stream (Of a) m ()
        f !i _ | n == i = S.liftIO $ Progress.complete pg
        f !i s = do
            r <- lift $ S.next s
            case r of
              Left ret -> S.liftIO $ Progress.complete pg
              Right (x, s') -> do
                  when (i `mod` step == 0) $ S.liftIO $ Progress.tickN pg step
                  S.yield x
                  f (i+1) s'
    f 0 s

runSim :: FilePath -> Options -> IO ()
runSim outPath (Opts {..}) = withSystemRandom $ \mwc -> do
    let boxSize = 20 *^ beamWidth
        dropletDiffusivity = 5.6e-3 -- nm^2/ns
        molDiffusivity = 0.122 -- nm^2/ns
        dropletSigma = sqrt $ msd dropletDiffusivity timeStep
        molSigma = sqrt $ msd molDiffusivity timeStep

        taus :: VU.Vector Int
        taus = VU.fromList $ nub $ map round
            $ logSpace (minLag / timeStep / realToFrac decimation) (maxLag / timeStep / realToFrac decimation) corrPts

        decimation :: Int
        decimation = ceiling $ minLag / timeStep

        steps :: Int
        steps = ceiling $ 20 * maxLag / timeStep

        nDroplets = 10
        dropletParams = DropletParams { bindingProb = 1e-5
                                      , unbindingProb = 1e-5
                                      , dropletSigma = dropletSigma
                                      , dropletRadius = 50
                                      , stickingRadius = 48
                                      , moleculeSigma = molSigma
                                      }

        spotMolCount = realToFrac nDroplets / product boxSize * spheroidVol (beamWidth ^. _x) (beamWidth ^. _z)
          where spheroidVol r1 r2 = 4*pi/3 * r1^2 * r2

    putStrLn $ "Run length: "++show steps++" steps"
    putStrLn $ "Decimation: "++show decimation
    putStrLn $ "Box size: "++show boxSize
    putStrLn $ "Droplet diffusivity: "++show dropletDiffusivity
    putStrLn $ "Molecule diffusivity: "++show molDiffusivity
    putStrLn $ "Params: "++show dropletParams
    putStrLn $ "<N>: "++show spotMolCount
    let dropletWalk :: Stream (Of (VU.Vector (Point V3 Length))) (RVarT IO) ()
        dropletWalk = do
            xs0 <- lift $ VU.replicateM nDroplets $ do
                dropletPosition <- pointInBox boxSize
                return Droplet { molPosition = origin, dropletPosition = dropletPosition, bound = False }
            let prop = dropletP boxSize dropletParams
            S.map (VU.map absMolPosition)
                $ decimate decimation
                -- $ S.take steps
                $ takeWithProgress steps
                $ propagateToStream (propMany prop) xs0

        walk :: Stream (Of (Log Double)) (RVarT IO) ()
        walk = case ModeWalkInCube of
          ModeDroplet -> S.map (VU.sum . VU.map (beamIntensity beamWidth)) dropletWalk
          ModeWalkInCube -> do
                xs0 <- lift $ VU.replicateM nDroplets $ pointInBox boxSize
                let prop = wanderInsideReflectiveCubeP boxSize dropletSigma
                S.map (VU.sum . VU.map (beamIntensity beamWidth))
                    $ decimate decimation
                    -- $ S.take steps
                    $ takeWithProgress steps
                    $ propagateToStream (propMany prop) xs0

        corr :: RVarT IO (VU.Vector (Int, Log Double))
        corr = do
            int <- streamToVector @VU.Vector
                walk
            let !norm = VU.sum int / realToFrac (VU.length int)
            return $! VU.map (\tau -> (tau, correlate tau int / norm)) taus

    runRVarT (S.mapM_ (S.liftIO . print) dropletWalk) mwc

    forM_ [0..] $ \i -> do
        let out = outPath++zeroPadded 4 i
        putStrLn out
        v <- runRVarT corr mwc
        writeFile out
          $ unlines $ map (\(x,y) -> show (realToFrac x * timeStep * realToFrac decimation) ++ "\t" ++ show (realToFrac y :: Double))
          $ VU.toList v

tests = [ reflectiveSphereStepIsInside ]

writeTrajectory :: FilePath -> [Point V3 Double] -> IO ()
writeTrajectory path = writeFile path . unlines . map (\(P (V3 x y z)) -> unwords [show x, show y, show z])

main :: IO ()
main = withSystemRandom $ \mwc -> do
    v <- runRVarT (propagateToVector 10000000 (randomWalkP @Identity 1) (P (V3 0 0 0))) mwc
    print $ VU.foldl' (\a (P x) -> a ^+^ x) zero (v :: VU.Vector (Point V3 Double))
    return ()
main' = Progress.displayConsoleRegions $ do
    --quickCheck reflectiveStepIsInside
    args <- execParser $ info (helper <*> options) mempty
    ncaps <- getNumCapabilities

    when False $ withSystemRandom $ \mwc -> do
        let x :: RVarT IO (VU.Vector (Point V3 Double))
            x = propagateToVector 100000 (wanderInsideSphereP 1e9 1) origin
        traj <- runRVarT x mwc
        writeFile "traj" $ unlines $ map (\(P (V3 x y z)) -> unwords [show x, show y, show z]) (VU.toList traj)
        writeFile "intensity" $ unlines $ map (show . beamIntensity (beamWidth args)) (VU.toList traj)

    forM_ [1..ncaps-1] $ \i -> forkIO $ runSim ("out/"++zeroPadded 2 i++"-") args
    runSim ("out/"++zeroPadded 2 0++"-") args
    return ()

-- | Render a number in zero-padded form
zeroPadded :: Int -> Int -> String
zeroPadded width n =
    let s = show n
    in replicate (width - length s) '0' ++ s

-- | Mean-squared displacement of a particle with the given diffusivity which
-- diffused for the given time.
msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt
{-# INLINEABLE msd #-}

-- | Compute the correlation function with zero boundaries at the given lag
correlate :: (VG.Vector v a, RealFrac a) => Int -> v a -> a
correlate tau xs = VG.sum $ VG.zipWith (*) xs (VG.drop tau xs)
{-# NOINLINE correlate #-}

-- | Generate a logarithmically-spaced
logSpace :: (RealFloat a, Enum a) => a -> a -> Int -> [a]
logSpace a b n = map exp [la,la+dx..lb]
  where la = log a
        lb = log b
        dx = (lb - la) / fromIntegral n
