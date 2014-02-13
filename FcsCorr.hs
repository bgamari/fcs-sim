{-# LANGUAGE FlexibleContexts #-}                

import FcsSim
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Control.Monad.State
import Data.Random
import Data.Functor.Identity
import Data.Traversable as T
import Data.Foldable as F
import Linear
import Control.Monad
import System.Random.MWC hiding (uniform)
import Control.Applicative
import Text.Printf
import Statistics.Sample
import Control.Concurrent.ParallelIO
import Control.Monad.Primitive.Class       

beamWidth = V3 400 400 1000
taus = VU.fromList $ map round
       $ logSpace 1 1000000 500
sigmas = replicate 50 6.5
boxSize = 15 *^ beamWidth

logSpace :: (Enum a, Floating a) => a -> a -> Int -> [a]
logSpace a b n = [exp x | x <- [log a,log a+dx..log b]]
  where dx = (log b - log a) / fromIntegral n

main = do
    corrs <- parallel
             $ map (\sigma->withSystemRandom $ asGenIO $ runRVarTWith id
                        $ correlateSample sigma taus
                   ) sigmas
    let corrStats :: VU.Vector (Double, Double)
        corrStats = V.fromList $ getZipList
                    $ fmap (meanVariance . VU.fromList)
                    $ traverse (ZipList . V.toList) corrs
    V.forM_ (V.zip taus corrStats) $ \(tau,(mean,var))->
        printf "%1.2f\t%1.2f\t%1.2f\n" (fromIntegral tau::Double) mean (sqrt var)

correlateSample :: (Monad m, MonadPrim (RVarT m))
                => Length -> VU.Vector Int -> RVarT m (VU.Vector Double)
correlateSample sigma taus = do
    traj <- evolveParticle boxSize sigma
    let intensity = V.map (beamIntensity beamWidth) traj
        norm = correlate 0 intensity
    return $ V.map (\tau->correlate tau intensity / norm) taus

correlate :: (V.Vector v a, RealFrac a) => Int -> v a -> a
correlate tau xs = V.sum $ V.zipWith (*) xs xs'
  where xs' = V.drop tau xs V.++ V.take tau xs
