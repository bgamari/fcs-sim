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

type Diffusivity = Double
type Time = Double
type Length = Double
type BeamSize = V3 Length
type BoxSize = V3 Length

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
beamIntensity w x = exp (negate alpha)
  where
    f wx xx = xx^2 / (2*wx^2)
    alpha = F.sum $ f <$> w <*> x
{-# INLINEABLE beamIntensity #-}

msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt

pointInBox :: BoxSize -> RVarT m (V3 Length)
pointInBox = traverse (\s->uniformT (-s/2) (s/2))

inBox :: BoxSize -> V3 Length -> Bool
inBox boxSize x = F.all id $ (\s x->abs x < s) <$> boxSize <*> x

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

instance PrimMonad m => PrimMonad (RVarT m) where
    type PrimState (RVarT m) = PrimState m
    primitive = lift . primitive

takeEvery :: Monad m => Int -> Pipe a a m r
takeEvery n = forever $ do
    await >>= yield
    P.drop n
