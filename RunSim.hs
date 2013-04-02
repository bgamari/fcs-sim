import System.Random.MWC hiding (uniform)
import Data.Random
import FcsSim
import Control.Applicative
import Control.Monad.State

main = withSystemRandom $ asGenIO $ \mwc->do
    runRVarT (runStateT evolve (pure 0)) mwc
