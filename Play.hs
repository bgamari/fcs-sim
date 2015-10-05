{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable
import Data.List (nub)
import Data.Functor.Identity
import Linear
import Linear.Affine
import Numeric.Log
import qualified Data.Vector.Fusion.Stream.Monadic as VSM
import qualified Data.Vector.Fusion.Bundle.Monadic as VBM
import qualified Data.Vector.Fusion.Bundle.Size as Size
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Control.Lens
import System.Random.MWC
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Options.Applicative
import GHC.Conc (getNumCapabilities, forkIO)

import Data.Random

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
    let phi = acos (2*x - 1)
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
    return $ r *^ normalize dir
  where
    dist = uniformT (-1) 1
{-# INLINEABLE step3D #-}

randomWalk :: Monad m => Length -> Point V3 Double -> VSM.Stream (RVarT m) (Point V3 Double)
randomWalk sigma = VSM.unfoldrM step
  where
    step !x = do
      dx <- step3D sigma
      let x' = x .+^ dx
      return $ Just (x', x')

-- | Produce a random walk inside a sphere with reflective boundary conditions
wanderInsideSphere :: (Monad m)
                   => Length -> Length -> Point V3 Double
                   -> VSM.Stream (RVarT m) (Point V3 Length)
wanderInsideSphere radius sigma = VSM.unfoldrM step
  where
    step !x = do
      dx <- step3D sigma
      let x' = reflectiveStep radius x dx
      return $ Just (x', x')

-- | @reflectiveStep r x0 dx@ is the result of a step from point @x0@
-- to @x0 + dx@ inside a reflective sphere of radius @r@.
reflectiveStep :: RealFloat a => a -> Point V3 a -> V3 a -> Point V3 a
reflectiveStep radius x0 dx
  | abs alpha < 1 =
    let P x' = x0 .+^ alpha *^ dx
        dx' = (1 - alpha) *^ dx
    in P x' .+^ reflect x' dx'
  | otherwise     = x0 .+^ dx
  where
    (_, alpha) = sphereIntercept radius x0 dx

-- | @reflect l v@ is the vector @v@ reflected across the plane normal to @l@.
reflect :: (Metric f, RealFrac a) => f a -> f a -> f a
reflect l v = 2 * (l `dot` v) / quadrance l *^ l ^-^ v

-- | @sphereIntercept r x0 dx@ is the values @alpha@ where
-- @x0 + alpha * dx@ falls on the surface of a sphere of radius @r@
-- centered at the origin. The first element is negative.
sphereIntercept :: RealFloat a => a -> Point V3 a -> V3 a -> (a, a)
sphereIntercept radius (P x0) dir =
    let [a, b] = quadratic (quadrance dir) (2 * x0 `dot` dir) (quadrance x0 - radius^2)
    in (a, b)

-- | Real solutions to a quadratic equation
quadratic :: RealFloat a => a -> a -> a -> [a]
quadratic a b c
  | discrim < 0 = []
  | otherwise   = [(-b + s) / 2 / a, (-b - s) / 2 / a]
  where
    discrim = b^2 - 4 * a * c
    s = sqrt discrim

beamIntensity :: BeamSize -> Point V3 Length -> Log Double
beamIntensity w (P x) = Exp (negate alpha / 2)
  where
    f wx xx = xx^2 / wx^2
    alpha = Data.Foldable.sum $ f <$> w <*> x
{-# INLINEABLE beamIntensity #-}

streamToVector :: (VG.Vector v a, PrimMonad m) => VSM.Stream m a -> m (v a)
streamToVector s = do
    mv <- VGM.munstream $ VBM.fromStream s Size.Unknown
    VG.unsafeFreeze mv

insideBox :: BoxSize -> Point V3 Length -> Bool
insideBox boxSize (P x) = Data.Foldable.all id $ (\s x->abs x < (s/2)) <$> boxSize <*> x
{-# INLINEABLE insideBox #-}

instance PrimMonad m => PrimMonad (RVarT m) where
  type PrimState (RVarT m) = PrimState m
  primitive f = lift $ primitive f

stokesEinstein :: Length   -- ^ radius
               -> Viscosity
               -> Diffusivity
stokesEinstein r eta = boltzmann * 300 / 6 / pi / eta / r
  where boltzmann = 1.38e-23 -- kg * m^2 / s^2 / K

waterVisc :: Viscosity
waterVisc = 1 -- kg * m / s

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
               <*> option auto ( short 't' <> long "time-step" <> value 1000 <> help "simulation timestep")
               <*> option auto ( short 'n' <> long "corr-pts" <> value 400 <> help "number of points to sample of correlation function")
               <*> option auto ( short 'l' <> long "min-lag" <> value 1000 <> help "minimum lag in nanoseconds")
               <*> option auto ( short 'L' <> long "max-lag" <> value 1e9 <> help "minimum lag in nanoseconds")

runSim :: FilePath -> Options -> IO ()
runSim outPath (Opts {..}) = withSystemRandom $ \mwc -> do
    let boxSize = 20 *^ beamWidth
        sigma = msd diffusivity timeStep
        taus = nub $ map round $ logSpace (minLag / timeStep) (maxLag / timeStep) corrPts

    let walk :: RVarT IO (VU.Vector (Point V3 Length))
        walk = walkInsideBox boxSize sigma

        corr :: RVarT IO [(Int, Log Double)]
        corr = do
            int <- VU.map (beamIntensity beamWidth) <$> walk
            return $ map (\tau -> (tau, correlate tau int)) taus

    forM_ [0..] $ \i -> do
        let out = outPath++zeroPadded 4 i
        putStrLn out
        v <- runRVarT corr mwc :: IO [(Int, Log Double)]
        writeFile out
          $ unlines $ map (\(x,y) -> show (realToFrac x * timeStep) ++ "\t" ++ show (ln y)) v

main :: IO ()
main = do
    args <- execParser $ info (helper <*> options) mempty
    ncaps <- getNumCapabilities
    forM_ [1..ncaps-1] $ \i -> forkIO $ runSim ("out/"++zeroPadded 2 i++"-") args
    runSim ("out/"++zeroPadded 2 0++"-") args
    return ()

-- | Render a number in zero-padded form
zeroPadded :: Int -> Int -> String
zeroPadded width n =
    let s = show n
    in replicate (width - length s) '0' ++ s

-- | Mean-squared displacement
msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt
{-# INLINEABLE msd #-}

pointInBox :: (Monad m) => BoxSize -> RVarT m (Point V3 Length)
pointInBox boxSize = P <$> traverse (\x -> uniformT (-x/2) (x/2)) boxSize

-- | Produce a random walk inside a simulation box until the path leaves
walkInsideBox :: (VG.Vector v (Point V3 Length), PrimMonad m)
              => BoxSize -> Length -> RVarT m (v (Point V3 Length))
walkInsideBox boxSize sigma = do
    x0 <- pointInBox boxSize
    let walk = VSM.takeWhile (insideBox boxSize) $ randomWalk sigma x0
    before <- streamToVector walk
    after <- streamToVector walk
    return $ VG.reverse before VG.++ after

-- | Compute the correlation function with zero boundaries at the given lag
correlate :: (VG.Vector v a, RealFrac a) => Int -> v a -> a
correlate tau xs = VG.sum $ VG.zipWith (*) xs (VG.drop tau xs)

-- | Generate a logarithmically-spaced
logSpace :: (RealFloat a, Enum a) => a -> a -> Int -> [a]
logSpace a b n = map exp [la,la+dx..lb]
  where la = log a
        lb = log b
        dx = (lb - la) / fromIntegral n
