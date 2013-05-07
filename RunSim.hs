import System.Random.MWC hiding (uniform)
import Data.Random
import FcsSim
import Control.Applicative
import Control.Monad.State
import Linear       

main = withSystemRandom $ asGenIO $ \mwc->do
    runRVarT (runStateT evolve (pure 0)) mwc

evolve :: StateT (V3 Double) (RVarT IO) ()
evolve = do
    replicateM_ (round 1e6) $
        evolveIntensity 1 1e-3 (pure 0.1) >>= lift . lift . print