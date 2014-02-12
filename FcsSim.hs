{-# LANGUAGE RecordWildCards, BangPatterns #-}

module FcsSim where

import Pipes
import Pipes.Prelude as P
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

step3D :: Monad m => Length -> RVarT m (V3 Double)
step3D sigma = do
    dir <- V3 <$> dist <*> dist <*> dist
    r <- normalT 0 sigma
    return $ r *^ normalize dir
  where
    dist = uniformT (-1) 1

unfold :: Monad m => m a -> Producer a m r
unfold m = forever $ lift m >>= yield

evolveDiffusion :: Monad m
                => Length -> Producer (V3 Double) (RVarT m) r
evolveDiffusion sigma = unfold (step3D sigma) >-> P.scan (^+^) zero id
{-# INLINEABLE evolveDiffusion #-}

beamIntensity :: V3 Length -> V3 Length -> Double
beamIntensity w x = F.product $ f <$> w <*> x
  where
    f wx xx = exp (negate $ xx^2 / (2*wx^2))
{-# INLINEABLE beamIntensity #-}

msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt
