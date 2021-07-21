{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import Text.Printf (printf)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified System.Clock as Clock

import qualified BitSet

data Benchmark = Benchmark
  { benchmarkLabel :: String
  , benchmarkCutoff :: Clock.TimeSpec
  , benchmarkRun :: IO SieveResult
  , benchmarkThreads :: Int
  , benchmarkFaithful :: Bool
  , benchmarkAlgorithm :: String
  , benchmarkBits :: Int
  }

runBenchmark :: Benchmark -> IO ()
runBenchmark bench = do
  (passes, duration, result) <- benchmark (benchmarkCutoff bench) (benchmarkRun bench)
  showResults bench passes duration result

-- | Count how many times the action can be run in the given duration,
-- and return the result of the last run.
benchmark :: Clock.TimeSpec -> IO r -> IO (Int, Clock.TimeSpec, r)
{-# NOINLINE benchmark #-}
benchmark duration action = do
  start <- Clock.getTime Clock.Monotonic
  loop 0 start (start + duration)
  where
    loop !counter start end = do
      result <- action
      now <- Clock.getTime Clock.Monotonic
      if now < end
        then loop (counter + 1) start end
        else pure (counter + 1, now - start, result)

showResults :: Benchmark -> Int -> Clock.TimeSpec -> SieveResult -> IO ()
showResults bench passes duration result = do
  printf "%s;%d;%.6f;%d;algorithm=%s,bits=%d,faithful=%s\n"
    (benchmarkLabel bench)
    passes
    durationSeconds
    (benchmarkThreads bench)
    (benchmarkAlgorithm bench)
    (benchmarkBits bench)
    (if benchmarkFaithful bench then "yes" else "no")
  where
    durationSeconds = realToFrac (Clock.toNanoSecs duration) / 1_000_000_000 :: Double

-- NOTE: The constructor fields of this type are intentionally lazy so that we don't pay
-- for their computation during the benchmark, just as it is the case in the CPP version.
data SieveResult = SieveResult
  { sieveResultSize :: Int -- ^ Size of the sieve
  , sieveResultNumPrimes :: Int -- ^ Number of primes found
  , sieveResultPrimes :: [Int] -- ^ Lazy list of primes
  }

-- | Historical data for validating our results - the number of primes
-- to be found under some limit, such as 168 primes under 1000
primeCounts :: [(Int, Int)]
primeCounts =
  [ (          10, 4         )
  , (         100, 25        )
  , (        1000, 168       )
  , (       10000, 1229      )
  , (      100000, 9592      )
  , (     1000000, 78498     )
  , (    10000000, 664579    )
  , (   100000000, 5761455   )
  , (  1000000000, 50847534  )
  , ( 10000000000, 455052511 )
  ]

isValid :: SieveResult -> Bool
isValid result
  = maybe False (== sieveResultNumPrimes result)
  $ lookup (sieveResultSize result) primeCounts
