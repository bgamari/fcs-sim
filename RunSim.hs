{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

import Control.Lens
import System.Random.MWC hiding (uniform)
import Data.Random
import FcsSim
import Control.Applicative
import Control.Monad.State
import Data.Traversable as T
import Data.Foldable as F
import Linear
import qualified Data.Vector as V

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
boxSize = 10 *^ beamWidth

evolve :: RVarT IO ()
evolve = do
    traj <- evolveParticle boxSize beamWidth sigma
    liftIO $ F.mapM_ print traj
 
pointInBox :: BoxSize -> RVarT m (V3 Length)
pointInBox boxSize = traverse (\s->uniformT (-s/2) (s/2)) boxSize

evolveUntilExit :: (Monad (m (RVarT n)), Monad n)
                => BoxSize
                -> V3 Length
                -> Length
                -> V3 Length
                -> m (RVarT n) ()
evolveUntilExit boxSize w sigma x0 =
    runWhile pred go
  where
    pred :: V3 Length -> Bool
    pred x = not $ F.any id
             $ (\s x->abs x > s) <$> boxSize <*> x
    go :: Monad m => LogT (V3 Length) (StateT (V3 Double) (RVarT m)) ()
    go = logAction (evolveDiffusion sigma)

evolveParticle :: Monad m
               => BoxSize -> V3 Length -> Length
               -> RVarT m (V.Vector (V3 Length))
evolveParticle boxSize w sigma = do
    x0 <- pointInBox boxSize
    pred <- execStateT (evolveUntilExit boxSize w sigma append x0) V.empty
    succ <- execStateT (evolveUntilExit boxSize w sigma append x0) V.empty
    return $ V.reverse pred V.++ succ
  where
    append x = modify (`V.snoc` x)

newtype LogT s m a = LogT (StateT (V.Vector s) m a)
                     deriving (Monad, Applicative, Functor, MonadTrans)

logValue :: s -> LogT s m ()
logValue s = modify' (`V.snoc` s)

logAction :: Monad m => m s -> LogT s m ()
logAction m = lift m >>= logValue
    
runLogT :: Monad m => LogT s m a -> m (a, V.Vector s)
runLogT (LogT m) = runStateT m V.empty

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = do
    s <- get        
    put $! f s

runWhile :: Monad m => (s -> Bool) -> m s -> m ()
runWhile pred step = go
  where
    go = do s <- step 
            if pred s
               then go
               else return ()
