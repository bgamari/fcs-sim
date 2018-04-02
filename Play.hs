{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Foldable
import Data.Semigroup ((<>), Sum(..))
import Data.List (nub)
import Data.Functor.Identity
import Linear
import Linear.Affine
import Numeric.Log
import Data.Random
import qualified Streaming as S
import qualified Streaming.Prelude as S
import Streaming (Of, Stream)

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector as V

import Control.Lens
import System.Random.MWC
import Data.Random.Distribution.Bernoulli
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import GHC.Conc (getNumCapabilities, forkIO)
import Options.Applicative
import Test.QuickCheck

import Reflection

data Spherical a = Spherical { _r      :: !a    -- ^ radial
                             , _theta  :: !a    -- ^ inclination
                             , _phi    :: !a    -- ^ azimuth
                             }
                 deriving (Show, Eq, Ord)

sphericalV3 :: RealFloat a => Iso' (Spherical a) (V3 a)
sphericalV3 = iso from to
  where
    from (Spherical r theta phi) = V3 x y z
      where
        x = r * sin theta * cos phi
        y = r * sin theta * sin phi
        z = r * cos theta
    to (V3 x y z) = Spherical r theta phi
      where
        r = sqrt (x^2 + y^2 + z^2)
        theta = acos (z / r)
        phi = atan2 y x
{-# INLINE sphericalV3 #-}

-- | Generate a Gaussian-distributed step in spherical coordinates
stepSpherical :: Monad m => Length -> RVarT m (Spherical Double)
stepSpherical sigma = do
    r <- normalT 0 sigma
    theta <- uniformT (-pi) pi
    x <- uniformT 0 1
    let !phi = acos (2*x - 1)
    return $ Spherical r theta phi
{-# INLINEABLE stepSpherical #-}

type Viscosity = Double     -- ^ centipoise
type Diffusivity = Double   -- ^ nm^2 / ns
type Time = Double          -- ^ nanoseconds
type Length = Double        -- ^ nanometers
type BeamSize = V3 Length
type BoxSize = V3 Length

-- | Generate a Gaussian-distributed step in Cartesian coordinates
step3D :: Monad m => Length -> RVarT m (V3 Double)
step3D sigma = do
    dir <- V3 <$> dist <*> dist <*> dist
    r <- normalT 0 sigma
    return $! r *^ normalize dir
  where
    dist = uniformT (-1) 1
{-# INLINEABLE step3D #-}

newtype Propagator m a = Propagator (a -> m a)

propagateToStream :: Monad m => Propagator m a -> a -> Stream (Of a) m r
propagateToStream (Propagator f) s0 = S.iterateM f (return $! s0)

propagateToVector :: (VG.Vector v a, Monad m) => Int -> Propagator m a -> a -> m (v a)
propagateToVector steps (Propagator f) s0 = VG.iterateNM steps f s0

propMany :: (Monad m, VG.Vector v a)
         => Propagator m a -> Propagator m (v a)
propMany (Propagator f) = Propagator (VG.mapM f)

randomWalkP :: Monad m => Length -> Propagator (RVarT m) (Point V3 Double)
randomWalkP sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $! x .+^ dx

-- | Draw a point from a given box.
pointInBox :: (Monad m) => BoxSize -> RVarT m (Point V3 Length)
pointInBox boxSize = P <$> traverse (\x -> uniformT (-x/2) (x/2)) boxSize

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

-- | Produce a random walk inside a sphere with reflective boundary conditions
wanderInsideSphereP :: (Monad m)
                    => Length -> Length
                    -> Propagator (RVarT m) (Point V3 Length)
wanderInsideSphereP radius sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $! reflectiveSphereStep radius x dx

wanderInsideReflectiveCubeP :: Monad m
                            => BoxSize -> Length
                            -> Propagator (RVarT m) (Point V3 Length)
wanderInsideReflectiveCubeP boxSize sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $! reflectiveCubeStep boxSize x dx

data Droplet = Droplet { molPosition :: !(Point V3 Length)
                       , dropletPosition :: !(Point V3 Length)
                       , bound :: !Bool
                       }

absMolPosition :: Droplet -> Point V3 Length
absMolPosition d = molPosition d .+^ unP (dropletPosition d)

data DropletParams = DropletParams { bindingProb    :: !Double   -- ^ binding probability when in sticky region
                                   , unbindingProb  :: !Double   -- ^ unbinding probability
                                   , dropletSigma   :: !Double   -- ^ droplet MSD
                                   , dropletRadius  :: !Double   -- ^ droplet radius
                                   , stickingRadius :: !Double   -- ^ sticking region
                                   , moleculeSigma  :: !Double   -- ^ molecule MSD
                                   }

dropletP :: Monad m
         => BoxSize
         -> DropletParams
         -> Propagator (RVarT m) Droplet
dropletP boxSize DropletParams{..} = Propagator $ \x -> do
    dxDroplet <- step3D dropletSigma
    molPosition' <- case bound x of
                      False -> do dx <- step3D moleculeSigma
                                  return $! reflectiveSphereStep dropletRadius (molPosition x) dx
                      True  -> return $! molPosition x
    stuck <- if | bound x -> not <$> bernoulliT unbindingProb
                | molPosition' `distance` origin > stickingRadius -> bernoulliT bindingProb
                | otherwise -> return False
    return $! Droplet { molPosition     = molPosition'
                      , dropletPosition = reflectiveCubeStep boxSize (dropletPosition x) dxDroplet
                      , bound           = stuck
                      }

-- | Gaussian beam intensity
beamIntensity :: BeamSize -> Point V3 Length -> Log Double
beamIntensity w (P x) = Exp (negate alpha / 2)
  where
    f wx xx = xx^2 / wx^2
    alpha = Data.Foldable.sum $ f <$> w <*> x
{-# INLINEABLE beamIntensity #-}

streamToVector :: forall v a m r. (VG.Vector v a, PrimMonad m)
               => Stream (Of a) m r -> m (v a)
streamToVector = VG.unfoldrM f
  where f s = either (const Nothing) Just <$> S.next s
{-# INLINE streamToVector #-}

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

runSim :: FilePath -> Options -> IO ()
runSim outPath (Opts {..}) = withSystemRandom $ \mwc -> do
    let boxSize = 20 *^ beamWidth
        dropletDiffusivity = 5.6e-3 -- nm^2/ns
        molDiffusivity = 0.122 -- nm^2/ns
        dropletSigma = sqrt $ msd dropletDiffusivity timeStep
        molSigma = sqrt $ msd molDiffusivity timeStep

        taus :: [Int]
        taus = nub $ map round $ logSpace (minLag / timeStep / realToFrac decimation) (maxLag / timeStep / realToFrac decimation) corrPts

        decimation :: Int
        decimation = ceiling $ minLag / timeStep

        steps :: Int
        steps = ceiling $ 20 * maxLag / timeStep

        nDroplets = 10

    putStrLn $ "Run length: "++show steps++" steps"
    putStrLn $ "Decimation: "++show decimation
    putStrLn $ "Box size: "++show boxSize
    putStrLn $ "Droplet diffusivity: "++show dropletDiffusivity
    putStrLn $ "Molecule diffusivity: "++show molDiffusivity
    let walk :: Stream (Of (Log Double)) (RVarT IO) ()
        walk = case ModeDroplet of
          ModeDroplet -> do
                xs0 <- lift $ VU.replicateM nDroplets $ do
                    dropletPosition <- pointInBox boxSize
                    return Droplet { molPosition = origin, dropletPosition = dropletPosition, bound = False }
                let prop = dropletP boxSize DropletParams { bindingProb = 1e-5
                                                          , unbindingProb = 1e-5
                                                          , dropletSigma = dropletSigma
                                                          , dropletRadius = 50
                                                          , stickingRadius = 48
                                                          , moleculeSigma = molSigma
                                                          }
                S.map (VG.sum . VG.map (beamIntensity beamWidth . absMolPosition))
                    $ decimate decimation
                    $ S.take steps
                    $ propagateToStream (propMany prop) xs0
          ModeWalkInCube -> do
                xs0 <- lift $ VU.replicateM nDroplets $ pointInBox boxSize
                let prop = wanderInsideReflectiveCubeP boxSize dropletSigma
                S.map (VU.sum . VU.map (beamIntensity beamWidth))
                    $ decimate decimation
                    $ S.take steps
                    $ propagateToStream (propMany prop) xs0

        corr :: RVarT IO [(Int, Log Double)]
        corr = do
            int <- streamToVector @VU.Vector walk
            return $ map (\tau -> (tau, correlate tau int)) taus

    forM_ [0..] $ \i -> do
        let out = outPath++zeroPadded 4 i
        putStrLn out
        v <- runRVarT corr mwc :: IO [(Int, Log Double)]
        writeFile out
          $ unlines $ map (\(x,y) -> show (realToFrac x * timeStep * realToFrac decimation) ++ "\t" ++ show (realToFrac y :: Double)) v

tests = [ reflectiveSphereStepIsInside ]

writeTrajectory :: FilePath -> [Point V3 Double] -> IO ()
writeTrajectory path = writeFile path . unlines . map (\(P (V3 x y z)) -> unwords [show x, show y, show z])

main :: IO ()
main = do
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

-- | Generate a logarithmically-spaced
logSpace :: (RealFloat a, Enum a) => a -> a -> Int -> [a]
logSpace a b n = map exp [la,la+dx..lb]
  where la = log a
        lb = log b
        dx = (lb - la) / fromIntegral n

derivingUnbox "Droplet"
    [t| Droplet -> (Point V3 Length, Point V3 Length, Bool) |]
    [| \(Droplet a b c) -> (a,b,c) |]
    [| \(a,b,c) -> Droplet a b c |]
