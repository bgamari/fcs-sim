{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies, FlexibleContexts #-}

module FcsSim where

import Pipes
import Pipes.Prelude as P
import Pipes.Vector
import Data.Random
import Linear
import Data.Foldable as F
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Control.Monad.Primitive.Class (MonadPrim(..))
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
beamIntensity w x = F.product $ f <$> w <*> x
  where
    f wx xx = exp (negate $ xx^2 / (2*wx^2))
{-# INLINEABLE beamIntensity #-}

msd :: Diffusivity -> Time -> Length
msd d dt = 6 * d * dt

pointInBox :: BoxSize -> RVarT m (V3 Length)
pointInBox boxSize = traverse (\s->uniformT (-s/2) (s/2)) boxSize

inBox :: BoxSize -> V3 Length -> Bool
inBox boxSize x = F.all id $ (\s x->abs x < s) <$> boxSize <*> x

evolveUntilExit :: Monad m
                => BoxSize -> Length -> V3 Double
                -> Producer (V3 Double) (RVarT m) ()
evolveUntilExit boxSize sigma start = do
    evolveDiffusion sigma
    >-> P.map (^+^ start)
    >-> P.takeWhile (inBox boxSize)
{-# INLINEABLE evolveUntilExit #-}    

evolveParticle :: (Monad m, MonadPrim (RVarT m))
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

instance MonadPrim m => MonadPrim (RVarT m) where
    type BasePrimMonad (RVarT m) = BasePrimMonad m
    liftPrim = lift . liftPrim

takeEvery :: Monad m => Int -> Pipe a a m r
takeEvery n = forever $ do
    await >>= yield
    P.drop n
