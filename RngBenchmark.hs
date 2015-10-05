module Main where

import Criterion.Main
import qualified System.Random.Mersenne as ME            -- mersenne-random +use_sse2
import qualified System.Random.Mersenne.Pure64 as MP64   -- mersenne-random-pure64
import qualified System.Random.TF as TF                  -- tf-random
import qualified System.Random as R                      -- random
import qualified System.Random.MWC as MWC                -- mwc-random
--import qualified GSL.Random.Gen as GSL                 -- gsl-random
--import qualified System.Random.AES as AES                -- intel-aes

import Control.Monad.State.Strict
import Control.Monad.Reader

main :: IO ()
main = do
  mtGen <- ME.newMTGen (Just 0xdeadbeef) -- only one allowed per process
  mwc <- MWC.create
  defaultMain
    [ bench "mersenne-random-pure64" $ whnf (evalState (sumInts <$> replicateM 100000 (state MP64.randomInt))) (MP64.pureMT 0xdeadbeef)
    , bench "mersenne-random" $ whnfIO $ runReaderT (sumInts <$> replicateM 100000 (ReaderT ME.random)) mtGen
    , bench "tf-random" $ whnf (evalState (sumInts <$> replicateM 100000 (state R.next))) (TF.seedTFGen (0xdeadbeef, 0, 0, 0))
    , bench "StdGen" $ whnf (evalState (sumInts <$> replicateM 100000 (state R.next))) (R.mkStdGen 0xdeadbeef)
    , bench "mwc-random" $ whnfIO $ runReaderT (sumInts <$> replicateM 100000 (ReaderT MWC.uniform)) mwc
    -- , bench "gsl-random" $ whnfIO $ runReaderT (sumInts <$> replicateM 100000 (ReaderT MWC.uniform)) `fmap` GSL.newRNG GSL.mt19937
    --, bench "intel-aes" $ whnf (evalState (sumInts <$> replicateM 100000 (state R.next))) (AES.mkAESGen 0xdeadbeef)
    ]

sumInts :: [Int] -> Int
sumInts = sum
