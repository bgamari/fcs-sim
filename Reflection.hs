{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Reflection where

import Debug.Trace
import Linear
import Linear.Affine
import Test.QuickCheck

-- | @reflectiveSphereStep r x0 dx@ is the result of a step from point @x0@
-- to @x0 + dx@ inside a reflective sphere of radius @r@.
reflectiveSphereStep :: (Show a, Epsilon a, RealFloat a)
                     => a -> Point V3 a -> V3 a -> Point V3 a
reflectiveSphereStep radius x0 dx
  | nearZero (quadrance dx) = x0
  | abs alpha < 1 =
    let P x' = x0 .+^ alpha *^ dx
        dx' = reflect x' ((1 - alpha) *^ dx)
    in reflectiveSphereStep radius (P x') dx'
  | otherwise     = x0 .+^ dx
  where
    (a, b) = sphereIntercept radius x0 dx
    alpha = max a b
{-# INLINE reflectiveSphereStep #-}

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (f a) => Arbitrary (Point f a) where
  arbitrary = P <$> arbitrary

reflectiveSphereStepIsInside :: Property
reflectiveSphereStepIsInside = property f
  where
    f :: Positive Double -> Point V3 Double -> V3 Double -> Bool
    f (Positive dr) x0 dir =
      let radius = norm x0 + dr
          x1 = reflectiveSphereStep radius x0 dir
      in norm x1 <= radius

reflectiveCubeStep :: RealFrac a => V3 a -> Point V3 a -> V3 a -> Point V3 a
reflectiveCubeStep (V3 sx sy sz) x0 dx = go (x0 .+^ dx)
  where
    go (P (V3 x y z)) = P (V3 (bound sx x) (bound sy y) (bound sz z))

    bound s x
      | x > s'         = s' - (x - s')
      | x < negate s'  = negate s' - (x - s')
      | otherwise      = x
      where s' = s/2

-- | @reflect n v@ is the vector @v@ reflected across the plane normal to @n@.
reflect :: (Metric f, RealFrac a) => f a -> f a -> f a
reflect n v = v ^-^ 2 * (v `dot` n) / quadrance n *^ n
{-# INLINE reflect #-}

-- | @sphereIntercept r x0 dx@ is the values @alpha@ where
-- @x0 + alpha * dx@ falls on the surface of a sphere of radius @r@
-- centered at the origin. The first element is negative.
sphereIntercept :: RealFloat a => a -> Point V3 a -> V3 a -> (a, a)
sphereIntercept radius (P x0) dir =
    case quadratic (quadrance dir) (2 * x0 `dot` dir) (quadrance x0 - radius^2) of
      TwoSolns a b -> (a, b)
      NoSoln       -> error "sphereIntercept"
{-# INLINE sphereIntercept #-}

data QuadraticSoln a = NoSoln | TwoSolns a a
                     deriving (Functor, Foldable)

-- | Real solutions to a quadratic equation
quadratic :: RealFloat a => a -> a -> a -> QuadraticSoln a
quadratic a b c
  | discrim < 0 = NoSoln
  | otherwise   = TwoSolns ((-b + s) / 2 / a) ((-b - s) / 2 / a)
  where
    discrim = b^2 - 4 * a * c
    s = sqrt discrim
{-# INLINE quadratic #-}
