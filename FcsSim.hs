{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module FcsSim where

import Pipes
import Pipes.Prelude as P
import Pipes.Vector
import Data.Random
import Linear
import Data.Foldable as F
import Control.Monad
import Control.Monad.Primitive (PrimMonad(..))
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import Control.Lens

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

type Diffusivity = Double
type Time = Double
type Length = Double
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

unfold :: Monad m => m a -> Producer a m r
unfold m = forever $ lift m >>= yield
{-# INLINEABLE unfold #-}

-- | Evolve a diffusive trajectory
evolveDiffusion :: Monad m
                => Length -> Producer (V3 Double) (RVarT m) r
evolveDiffusion sigma = unfold (step3D sigma) >-> P.scan (^+^) zero id
{-# INLINEABLE evolveDiffusion #-}

--evolveSphereDiffusion :: Monad m => Length -> Length -> Producer (V3 Double) (RVarT m) r
--evolveSphereDiffusion rMax sigma = unfold (stepSpherical sigma)
--{-# INLINEABLE evolveSphereDiffusion #-}

beamIntensity :: V3 Length -> V3 Length -> Double
beamIntensity w x = exp (negate alpha / 2)
  where
    f wx xx = xx^2 / wx^2
    alpha = F.sum $ f <$> w <*> x
{-# INLINEABLE beamIntensity #-}

msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt
{-# INLINEABLE msd #-}

pointInBox :: BoxSize -> RVarT m (V3 Length)
pointInBox = traverse (\s->uniformT (-s/2) (s/2))
{-# INLINEABLE pointInBox #-}

inBox :: BoxSize -> V3 Length -> Bool
inBox boxSize x = F.all id $ (\s x->abs x < (s/2)) <$> boxSize <*> x
{-# INLINEABLE inBox #-}

evolveUntilExit :: Monad m
                => BoxSize -> Length -> V3 Double
                -> Producer (V3 Double) (RVarT m) ()
evolveUntilExit boxSize sigma start =
    evolveDiffusion sigma
    >-> P.map (^+^ start)
    >-> P.takeWhile (inBox boxSize)
{-# INLINEABLE evolveUntilExit #-}

evolveParticle :: (Monad m, PrimMonad m)
               => BoxSize -> Length
               -> RVarT m (VS.Vector (V3 Length))
evolveParticle boxSize sigma = do
    x0 <- pointInBox boxSize
    va <- runToVector $ runEffect
          $ hoist lift (evolveUntilExit boxSize sigma x0) >-> toVector
    vb <- runToVector $ runEffect
          $ hoist lift (evolveUntilExit boxSize sigma x0) >-> toVector
    return $ V.reverse va V.++ vb
{-# INLINEABLE evolveParticle #-}

metropolisStep :: RVarT m a -> (a -> a -> Bool) -> a -> RVarT m a
metropolisStep proposal accept x0 = do
    x1 <- proposal
    return $ if accept x0 x1
               then x1
               else x0
{-# INLINEABLE metropolisStep #-}

instance PrimMonad m => PrimMonad (RVarT m) where
    type PrimState (RVarT m) = PrimState m
    primitive = lift . primitive

takeEvery :: Monad m => Int -> Pipe a a m r
takeEvery n = forever $ do
    await >>= yield
    P.drop n
{-# INLINEABLE takeEvery #-}
