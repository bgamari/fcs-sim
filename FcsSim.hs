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

step3D :: Monad m
       => Diffusivity -> Time -> RVarT m (V3 Double)
step3D d dt = do
    dir <- V3 <$> dist <*> dist <*> dist
    r <- normalT 0 (6*d*dt)
    return $ r *^ normalize dir
  where
    dist = uniformT (-1) 1
       
evolveDiffusion :: Monad m
                => Diffusivity -> Time -> V3 Double -> RVarT m (V3 Double)
evolveDiffusion d dt x = do
    dx <- step3D d dt
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
