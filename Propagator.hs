{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Propagator where

import Linear
import Linear.Affine
import Data.Random
import qualified Streaming.Prelude as S
import Streaming (Of, Stream)

import qualified Data.Vector.Generic as VG
import Data.Vector.Unboxed.Deriving

import Data.Random.Distribution.Bernoulli
import Control.Monad.Primitive

import Types
import Reflection

-- | Generate a Gaussian-distributed step in spherical coordinates
stepSpherical :: Monad m => Length -> RVarT m (Spherical Double)
stepSpherical sigma = do
    r <- normalT 0 sigma
    theta <- uniformT (-pi) pi
    x <- uniformT 0 1
    let !phi = acos (2*x - 1)
    return $ Spherical r theta phi
{-# INLINEABLE stepSpherical #-}

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
                   deriving (Show)

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

streamToVector :: forall v a m r. (VG.Vector v a, PrimMonad m)
               => Stream (Of a) m r -> m (v a)
streamToVector = VG.unfoldrM f
  where f s = either (const Nothing) Just <$> S.next s
{-# INLINE streamToVector #-}

derivingUnbox "Droplet"
    [t| Droplet -> (Point V3 Length, Point V3 Length, Bool) |]
    [| \(Droplet a b c) -> (a,b,c) |]
    [| \(a,b,c) -> Droplet a b c |]
