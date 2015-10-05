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
               <*> option auto ( short 'n' <> long "corr-pts" <> value 1000 <> help "number of points to sample of correlation function")
               <*> option auto ( short 'l' <> long "min-lag" <> value 1 <> help "minimum lag in seconds")
               <*> option auto ( short 'L' <> long "max-lag" <> value 10000000 <> help "minimum lag in seconds")

runSim :: FilePath -> Options -> IO ()
runSim outPath (Opts {..}) = withSystemRandom $ \mwc -> do
    let boxSize = 20 *^ beamWidth
        sigma = msd diffusivity timeStep
        taus = nub $ map round $ logSpace (minLag / timeStep) (maxLag / timeStep) 2000
    let corr :: RVarT IO [(Int, Log Double)]
        corr = do
            int <- VU.map (beamIntensity beamWidth) <$> walkInsideBox boxSize sigma
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
