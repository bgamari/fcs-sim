{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

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

-- | Generate a Gaussian-distributed step in spherical coordinates
stepSpherical :: Monad m => Length -> RVarT m (Spherical Double)
stepSpherical sigma = do
    r <- normalT 0 sigma
    theta <- uniformT (-pi) pi
    x <- uniformT 0 1
    let phi = acos (2*x - 1)
    return $ Spherical r theta phi
{-# INLINEABLE stepSpherical #-}

randomWalk :: Monad m => Length -> Point V3 Double -> VSM.Stream (RVarT m) (Point V3 Double)
randomWalk sigma = VSM.unfoldrM step
  where
    step !x = do
      dx <- step3D sigma
      let x' = x .+^ dx
      return $ Just (x', x')

beamIntensity :: BeamSize -> Point V3 Length -> Log Double
beamIntensity w (P x) = Exp (negate alpha)
  where
    f wx xx = xx^2 / (2*wx^2)
    alpha = Data.Foldable.sum $ f <$> w <*> x
{-# INLINEABLE beamIntensity #-}

streamToVector :: (VG.Vector v a, PrimMonad m) => VSM.Stream m a -> m (v a)
streamToVector s = do
    mv <- VGM.munstream $ VBM.fromStream s Size.Unknown
    VG.unsafeFreeze mv

insideBox :: BoxSize -> Point V3 Length -> Bool
insideBox boxSize (P x) = Data.Foldable.all id $ (\s x->abs x < s) <$> boxSize <*> x
{-# INLINEABLE insideBox #-}

instance PrimMonad m => PrimMonad (RVarT m) where
  type PrimState (RVarT m) = PrimState m
  primitive f = lift $ primitive f

main :: IO ()
main = withSystemRandom $ \mwc -> do
    let beamWidth = V3 400 400 1000
        boxSize = 20 *^ beamWidth
        timeStep = 10 -- ns
        diffusivity = 6.5^2 / 6 / 10
        sigma = msd diffusivity timeStep
        taus = nub $ map round $ logSpace 1 1e9 2000

    let corr :: RVarT IO [(Int, Log Double)]
        corr = do
            int <- VU.map (beamIntensity beamWidth) <$> walkInsideBox boxSize sigma
            return $ map (\tau -> (tau, correlate tau int)) taus

    forM_ [0..] $ \i -> do
        print i
        v <- runRVarT corr mwc :: IO [(Int, Log Double)]
        writeFile ("out/"++show i) $ unlines $ map (\(x,y) -> show (realToFrac x * timeStep) ++ "\t" ++ show (ln y)) v

    --v <- runRVarT (walkInsideBox boxSize sigma :: RVarT IO (VU.Vector (Point V3 Double))) mwc
    --putStrLn $ unlines $ map (show . beamIntensity beamWidth) $ VG.toList v
    --putStrLn $ unlines $ map (foldMap (flip shows " ")) $ VG.toList v
    return ()

-- | Mean-squared displacement
msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt
{-# INLINEABLE msd #-}

pointInBox :: (Monad m) => BoxSize -> RVarT m (Point V3 Length)
pointInBox boxSize = P <$> traverse (\x -> uniformT (negate x) x) boxSize

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
