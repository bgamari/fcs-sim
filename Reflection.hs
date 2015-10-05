{-# LANGUAGE FlexibleContexts #-}
module Reflection where

import Debug.Trace
import Linear
import Linear.Affine
import Test.QuickCheck

-- | @reflectiveStep r x0 dx@ is the result of a step from point @x0@
-- to @x0 + dx@ inside a reflective sphere of radius @r@.
reflectiveStep :: (Show a, Epsilon a, RealFloat a)
               => a -> Point V3 a -> V3 a -> Point V3 a
reflectiveStep radius x0 dx
  | nearZero (quadrance dx) = x0
  | abs alpha < 1 =
    let P x' = x0 .+^ alpha *^ dx
        dx' = reflect x' ((1 - alpha) *^ dx)
    in reflectiveStep radius (P x') dx'
  | otherwise     = x0 .+^ dx
  where
    (a, b) = sphereIntercept radius x0 dx
    alpha = max a b
{-# INLINE reflectiveStep #-}

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (f a) => Arbitrary (Point f a) where
  arbitrary = P <$> arbitrary

reflectiveStepIsInside :: Property
reflectiveStepIsInside = property f
  where
    f :: Positive Double -> Point V3 Double -> V3 Double -> Bool
    f (Positive dr) x0 dir =
      let radius = norm x0 + dr
          x1 = reflectiveStep radius x0 dir
      in norm x1 <= radius

-- | @reflect n v@ is the vector @v@ reflected across the plane normal to @n@.
reflect :: (Metric f, RealFrac a) => f a -> f a -> f a
reflect n v = v ^-^ 2 * (v `dot` n) / quadrance n *^ n
{-# INLINE reflect #-}

-- | @sphereIntercept r x0 dx@ is the values @alpha@ where
-- @x0 + alpha * dx@ falls on the surface of a sphere of radius @r@
-- centered at the origin. The first element is negative.
sphereIntercept :: RealFloat a => a -> Point V3 a -> V3 a -> (a, a)
sphereIntercept radius (P x0) dir
  | [a, b] <- xs = (a, b)
  | otherwise    = error "sphereIntercept"
  where
    xs = quadratic (quadrance dir) (2 * x0 `dot` dir) (quadrance x0 - radius^2)
{-# INLINE sphereIntercept #-}

-- | Real solutions to a quadratic equation
quadratic :: RealFloat a => a -> a -> a -> [a]
quadratic a b c
  | discrim < 0 = []
  | otherwise   = [(-b + s) / 2 / a, (-b - s) / 2 / a]
  where
    discrim = b^2 - 4 * a * c
    s = sqrt discrim
{-# INLINE quadratic #-}
