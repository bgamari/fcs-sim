{-# LANGUAGE RecordWildCards, BangPatterns #-}

module FcsSim where

import Data.Random
import Linear
import Data.Foldable as F
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative

type Diffusivity = Double
type Time = Double

-- theta is azimuth (0, 2*pi), phi is inclination (0, pi)
sphericalToCartesian :: Double -> Double -> Double -> V3 Double
sphericalToCartesian r theta phi = V3 x y z
  where x = r * sin phi * cos theta
        y = r * sin phi * sin theta
        z = r * cos phi

evolveDiffusion :: Monad m
                => Diffusivity -> Time -> V3 Double -> RVarT m (V3 Double)
evolveDiffusion d dt x = do
    r <- normalT 0 (6*d*dt)
    phi <- uniformT 0 pi
    theta <- uniformT 0 (2*pi)
    let dx = sphericalToCartesian r theta phi
    return $! x ^+^ dx

beamIntensity :: V3 Double -> V3 Double -> Double
beamIntensity w x = F.product $ f <$> w <*> x
  where
    f wx xx = exp (negate $ xx^2 / (2*wx^2))

evolveIntensity :: Monad m => Diffusivity -> Time -> V3 Double
                -> StateT (V3 Double) (RVarT m) Double
evolveIntensity d dt beamWidth = do
    x <- get
    x' <- lift $ evolveDiffusion d dt x
    put $! x'
    return $! beamIntensity beamWidth x'
