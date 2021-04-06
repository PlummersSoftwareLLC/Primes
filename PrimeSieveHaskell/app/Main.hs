{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Text.Printf (printf)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified System.Clock as Clock

main :: IO ()
main = do
  let durationCutoff = Clock.TimeSpec { Clock.sec = 5, Clock.nsec = 0 }
  let sieveSize = 1_000_000

  (passes, duration, result) <- benchmark durationCutoff (sieve sieveSize)
  showResults passes duration result

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

showResults :: Int -> Clock.TimeSpec -> SieveResult -> IO ()
showResults passes duration result = do
  printf "Passes: %d, Time: %.6f, Avg: %.6f, Limit: %d, Count1: %d, Count2: %d, Valid: %s\n"
    passes
    durationSeconds
    (durationSeconds / realToFrac passes)
    (sieveResultSize result)
    (sieveResultNumPrimes result)
    (length $ sieveResultPrimes result)
    (show $ isValid result)
  where
    durationSeconds = realToFrac (Clock.toNanoSecs duration) / 1_000_000_000 :: Double

-- NOTE: The constructor fields of this type are intentionally lazy so that we don't pay
-- for their computation during the benchmark, just as it is the case in the CPP version.
data SieveResult = SieveResult
  { sieveResultSize :: Int -- ^ Size of the sieve
  , sieveResultNumPrimes :: Int -- ^ Number of primes found
  , sieveResultPrimes :: [Int] -- ^ Lazy list of primes
  }

sieve :: Int -> IO SieveResult
{-# NOINLINE sieve #-}
sieve sieveSize = do
  let q = floor $ sqrt @Double $ realToFrac sieveSize
  primes <- VUM.replicate sieveSize True

  let
    go !factor
      | factor > q = pure ()
      | otherwise = do
        factor' <- findNext factor
        eliminate factor'
        go (factor' + 2)

    findNext !num = VUM.read primes num >>= \case
      True -> pure num
      False -> findNext (num + 2)

    eliminate !factor = do
      let
        loop num
          | num < sieveSize = do
            VUM.write primes num False
            loop (num + 2 * factor)
          | otherwise = pure ()
      loop (factor * factor)

  go 3
  sieveSet <- VU.unsafeFreeze primes

  pure SieveResult
    { sieveResultSize = sieveSize
    , sieveResultNumPrimes = if sieveSize < 2
        then 0
        else 1 + VU.length (VU.ifilter (\index isPrime -> odd index && isPrime) $ VU.drop 2 sieveSet)
    , sieveResultPrimes =
        if sieveSize < 2
          then []
          else 2 : VU.toList
            ( VU.map fst
            $ VU.filter (\(index, isPrime) -> odd index && isPrime)
            $ VU.drop 2
            $ VU.indexed sieveSet)
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
