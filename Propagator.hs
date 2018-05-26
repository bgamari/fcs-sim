{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Propagator where

import Control.DeepSeq
import Control.Lens
import Control.Monad.Primitive.Class
import Data.Foldable
import System.Random.MWC.Monad
import System.Random.MWC.Distributions.Monad
import Linear
import Linear.Affine
import qualified Streaming.Prelude as S
import Streaming (Of, Stream, lift)

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Generic.Sized as VGS
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving

import Control.Monad.Primitive

import HomArray
import Types
import Reflection

instance PrimMonad m => PrimMonad (Rand m) where
    type PrimState (Rand m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

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
    s <- stepSpherical sigma
    return $! view sphericalV3 s
{-
step3D sigma = do
    dir <- V3 <$> dist <*> dist <*> dist
    r <- normal 0 sigma
    return $! r *^ normalize dir
  where
    dist = uniformR (-1, 1)
    -- N.B. normalize doesn't inline
    normalize x = x ^* reciprocal (sqrt $ x `dot` x)
-}
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

propMany :: (Monad m, VG.Vector VU.Vector a)
         => Propagator m a -> Propagator m (HomArray n a)
propMany (Propagator f) = Propagator (fmap HomArray . VGS.mapM f . unHomArray)
{-# INLINEABLE propMany #-}

randomWalkP :: MonadPrim m => Length -> Propagator (Rand m) (Point V3 Double)
randomWalkP sigma = Propagator $ \x -> do
    dx <- step3D sigma
    return $! x .+^ dx
{-# INLINEABLE randomWalkP #-}

-- | Draw a point from a given box.
pointInBox :: (MonadPrim m) => BoxSize -> Rand m (Point V3 Length)
pointInBox boxSize = P <$!!> traverse (\x -> uniformR (-x/2, x/2)) boxSize
{-# INLINEABLE pointInBox #-}

-- | Draw a point from the boundary of the given box.
pointOnBox :: (MonadPrim m) => BoxSize -> Rand m (Point V3 Length)
pointOnBox boxSize = do
    face <- uniformR (0,2)
    case face :: Int of
      0 -> do y <- uniformR (negate $ boxSize ^. _y, boxSize ^. _y)
              z <- uniformR (negate $ boxSize ^. _z, boxSize ^. _z)
              s <- sign
              let x = s $ (boxSize ^. _x)
              return $ P $ V3 x y z
      1 -> do x <- uniformR (negate $ boxSize ^. _x, boxSize ^. _x)
              z <- uniformR (negate $ boxSize ^. _z, boxSize ^. _z)
              s <- sign
              let y = s $ (boxSize ^. _y)
              return $ P $ V3 x y z
      2 -> do x <- uniformR (negate $ boxSize ^. _x, boxSize ^. _x)
              y <- uniformR (negate $ boxSize ^. _y, boxSize ^. _y)
              s <- sign
              let z = s $ (boxSize ^. _z)
              return $ P $ V3 x y z
      _ -> fail "pointOnBox: impossible"
  where
    sign = f <$> bernoulli 0.5
      where f True = id
            f False = negate
{-# INLINEABLE pointOnBox #-}

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

wanderInsideCubeP :: (MonadPrim m)
                  => BoxSize -> Length
                  -> Propagator (Rand m) (Point V3 Length)
wanderInsideCubeP boxSize sigma = Propagator $ \x -> do
    dx <- step3D sigma
    let x' = x .+^ dx
    if insideBox boxSize x'
      then return $!! reflectiveCubeStep boxSize x dx
      else pointOnBox boxSize
{-# INLINEABLE wanderInsideCubeP #-}

insideBox :: BoxSize -> Point V3 Length -> Bool
insideBox boxSize (P x) = Data.Foldable.and $ (\s y->abs y < (s/2)) <$> boxSize <*> x
{-# INLINEABLE insideBox #-}

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
streamToVector = VG.unfoldrM f where f s = either (const Nothing) Just <$> S.next s
{-
streamToVector m0 = do
    acc <- VGM.new 1024
    go acc 0 m0
  where
    go :: VG.Mutable v (PrimState m) a -> Int -> Stream (Of a) m r -> m (v a)
    go acc !i m
      | i >= len = do
          acc' <- VGM.unsafeGrow acc (2*len)
          go acc' i m
      | otherwise = do
          res <- S.next m
          case res of
            Left _ -> VG.unsafeFreeze acc
            Right (x, m') -> do
                VGM.unsafeWrite acc i x
                go acc (i+1) m'
      where len = VGM.length acc
-}
{-# INLINE streamToVector #-}

derivingUnbox "Droplet"
    [t| Droplet -> (Point V3 Length, Point V3 Length, Bool) |]
    [| \(Droplet a b c) -> (a,b,c) |]
    [| \(a,b,c) -> Droplet a b c |]
