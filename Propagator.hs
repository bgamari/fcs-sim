{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Propagator where

import Control.DeepSeq
import Control.Monad.Primitive.Class
import System.Random.MWC.Monad
import System.Random.MWC.Distributions.Monad
import Linear
import Linear.Affine
import qualified Streaming.Prelude as S
import Streaming (Of, Stream, lift)

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed.Deriving

import Control.Monad.Primitive

import Types
import Reflection

instance PrimMonad m => PrimMonad (Rand m) where
    type PrimState (Rand m) = PrimState m
    primitive m = lift $ primitive m

-- | Generate a Gaussian-distributed step in spherical coordinates
stepSpherical :: MonadPrim m => Length -> Rand m (Spherical Double)
stepSpherical sigma = do
    r <- normal 0 sigma
    theta <- uniformR (-pi, pi)
    x <- uniformR (0, 1)
    let !phi = acos (2*x - 1)
    return $ Spherical r theta phi
{-# INLINEABLE stepSpherical #-}

-- | Generate a Gaussian-distributed step in Cartesian coordinates
step3D :: MonadPrim m => Length -> Rand m (V3 Double)
step3D sigma = do
    dir <- V3 <$> dist <*> dist <*> dist
    r <- normal 0 sigma
    return $! r *^ normalize dir
  where
    dist = uniformR (-1, 1)
{-# INLINEABLE step3D #-}

newtype Propagator m a = Propagator (a -> m a)

propagateToStream :: Monad m => Propagator m a -> a -> Stream (Of a) m r
propagateToStream (Propagator f) s0 = S.iterateM f (return $! s0)
{-# INLINEABLE propagateToStream #-}

propagateToVector :: (VG.Vector v a, PrimMonad m) => Int -> Propagator m a -> a -> m (v a)
-- Naively one might write,
--   propagateToVector steps (Propagator f) s0 = VG.iterateNM steps f s0
-- However, this fails to fuse due to https://github.com/haskell/vector/issues/208
propagateToVector steps (Propagator f) s0 = do
    v <- VGM.unsafeNew steps
    let go 0 _ = VG.freeze v
        go n s = do
            s' <- f s
            VGM.unsafeWrite v (steps-n) s'
            go (n-1) s'
    go steps s0
{-# INLINEABLE propagateToVector #-}

--propagateToVector steps (Propagator f) s0 = return VG.empty

propMany :: (Monad m, VG.Vector v a)
         => Propagator m a -> Propagator m (v a)
propMany (Propagator f) = Propagator (VG.mapM f)

randomWalkP :: MonadPrim m => Length -> Propagator (Rand m) (Point V3 Double)
randomWalkP sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $! x .+^ dx
{-# INLINEABLE randomWalkP #-}

-- | Draw a point from a given box.
pointInBox :: (MonadPrim m) => BoxSize -> Rand m (Point V3 Length)
pointInBox boxSize = P <$!!> traverse (\x -> uniformR (-x/2, x/2)) boxSize
{-# INLINEABLE pointInBox #-}

-- | Produce a random walk inside a sphere with reflective boundary conditions
wanderInsideSphereP :: (MonadPrim m)
                    => Length -> Length
                    -> Propagator (Rand m) (Point V3 Length)
wanderInsideSphereP radius sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $!! reflectiveSphereStep radius x dx
{-# INLINEABLE wanderInsideSphereP #-}

wanderInsideReflectiveCubeP :: (MonadPrim m)
                            => BoxSize -> Length
                            -> Propagator (Rand m) (Point V3 Length)
wanderInsideReflectiveCubeP boxSize sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $!! reflectiveCubeStep boxSize x dx
{-# INLINEABLE wanderInsideReflectiveCubeP #-}

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
                   deriving (Show)

dropletP :: (MonadPrim m)
         => BoxSize
         -> DropletParams
         -> Propagator (Rand m) Droplet
dropletP boxSize DropletParams{..} = Propagator $ \x -> do
    dxDroplet <- step3D dropletSigma
    molPosition' <- case bound x of
                      False -> do dx <- step3D moleculeSigma
                                  return $! reflectiveSphereStep dropletRadius (molPosition x) dx
                      True  -> return $! molPosition x
    stuck <- if | bound x -> not <$> bernoulli unbindingProb
                | molPosition' `distance` origin > stickingRadius -> bernoulli bindingProb
                | otherwise -> return False
    return $! Droplet { molPosition     = molPosition'
                      , dropletPosition = reflectiveCubeStep boxSize (dropletPosition x) dxDroplet
                      , bound           = stuck
                      }
{-# INLINEABLE dropletP #-}

streamToVector :: forall v a m r. (VG.Vector v a, PrimMonad m)
               => Stream (Of a) m r -> m (v a)
streamToVector = VG.unfoldrM f
  where f s = either (const Nothing) Just <$> S.next s
--streamToVector m = do
--    S.length m
--    return $ VG.empty
{-# INLINEABLE streamToVector #-}

derivingUnbox "Droplet"
    [t| Droplet -> (Point V3 Length, Point V3 Length, Bool) |]
    [| \(Droplet a b c) -> (a,b,c) |]
    [| \(a,b,c) -> Droplet a b c |]
