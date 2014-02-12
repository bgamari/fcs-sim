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
type Length = Double

step3D :: Monad m
       => Length -> RVarT m (V3 Double)
step3D sigma = do
    dir <- V3 <$> dist <*> dist <*> dist
    r <- normalT 0 sigma
    return $ r *^ normalize dir
  where
    dist = uniformT (-1) 1
  
evolveDiffusion :: Monad m
                => Length -> StateT (V3 Double) (RVarT m) (V3 Double)
evolveDiffusion sigma = do
    x <- get                
    dx <- lift $ step3D sigma
    let x' = x ^+^ dx
    put $! x'
    return x' 
{-# INLINEABLE evolveDiffusion #-}

beamIntensity :: V3 Length -> V3 Length -> Double
beamIntensity w x = F.product $ f <$> w <*> x
  where
    f wx xx = exp (negate $ xx^2 / (2*wx^2))

evolveIntensity :: Monad m => Length -> V3 Double
                -> StateT (V3 Double) (RVarT m) Double
evolveIntensity sigma beamWidth = do
    evolveDiffusion sigma
    beamIntensity beamWidth <$> get
{-# INLINEABLE evolveIntensity #-}

stepSize :: Diffusivity -> Time -> Length
stepSize d dt = 6 * d * dt
