{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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

-- units:
--   length:   nm
--   time:     ns

-- \delta T = 10 ns
-- sigma = 0.65 nm

main = withSystemRandom $ asGenIO $ \mwc->do
    runRVarT evolve mwc

-- | Step length
sigma = 6.5 -- nm

beamWidth = V3 400 400 1000

-- | box size
boxSize = 15 *^ beamWidth

evolve :: RVarT IO ()
evolve = do
    traj <- evolveParticle boxSize sigma
    liftIO $ V.mapM_ print traj
