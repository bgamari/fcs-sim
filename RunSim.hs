{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving, FlexibleContexts, TypeFamilies #-}

import Control.Monad.Primitive.Class (MonadPrim(..))
import Control.Monad.Morph
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Vector
import Control.Lens
import System.Random.MWC hiding (uniform)
import Data.Random
import FcsSim
import Control.Applicative
import Control.Monad.State
import Data.Traversable as T
import Data.Foldable as F
import Linear
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS

-- units:
--   length:   nm
--   time:     ns

-- \delta T = 10 ns
-- sigma = 0.65 nm

type BoxSize = V3 Length

main = withSystemRandom $ asGenIO $ \mwc->do
    runRVarT evolve mwc

-- | Step length    
sigma = 6.5 -- nm

beamWidth = V3 400 400 1000

-- | box size      
boxSize = 15 *^ beamWidth

evolve :: RVarT IO ()
evolve = do
    traj <- evolveParticle boxSize beamWidth sigma
    liftIO $ V.mapM_ print traj
 
pointInBox :: BoxSize -> RVarT m (V3 Length)
pointInBox boxSize = traverse (\s->uniformT (-s/2) (s/2)) boxSize

inBox :: BoxSize -> V3 Length -> Bool
inBox boxSize x = F.all id $ (\s x->abs x < s) <$> boxSize <*> x

type BeamSize = V3 Length

evolveUntilExit :: Monad m
                => BoxSize -> Length -> V3 Double
                -> Producer (V3 Double) (RVarT m) ()
evolveUntilExit boxSize sigma start = do
    evolveDiffusion sigma
    >-> P.map (^+^ start)
    >-> P.takeWhile (inBox boxSize)

evolveParticle :: (Monad m, MonadPrim (RVarT m))
               => BoxSize -> V3 Length -> Length
               -> RVarT m (VS.Vector (V3 Length))
evolveParticle boxSize w sigma = do
    x0 <- pointInBox boxSize
    va <- runToVector $ runEffect
          $ hoist lift (evolveUntilExit boxSize sigma x0) >-> toVector
    vb <- runToVector $ runEffect
          $ hoist lift (evolveUntilExit boxSize sigma x0) >-> toVector
    return $ V.reverse va V.++ vb

instance MonadPrim m => MonadPrim (RVarT m) where
    type BasePrimMonad (RVarT m) = BasePrimMonad m
    liftPrim = lift . liftPrim

takeEvery :: Monad m => Int -> Pipe a a m r
takeEvery n = forever $ do
    await >>= yield
    P.drop n
