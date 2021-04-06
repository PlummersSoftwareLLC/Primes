{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified System.Clock as Clock

import Lib
import qualified BitSet

main :: IO ()
main = do
  let durationCutoff = Clock.TimeSpec { Clock.sec = 5, Clock.nsec = 0 }
  let sieveSize = 1_000_000

  runBenchmark sieveSize durationCutoff sieveBitSet

sieveBitSet :: Int -> IO SieveResult
{-# NOINLINE sieveBitSet #-}
sieveBitSet sieveSize = do
  let q = floor $ sqrt @Double $ realToFrac sieveSize
  primes <- BitSet.new sieveSize True

  let
    go !factor
      | factor > q = pure ()
      | otherwise = do
        factor' <- findNext factor
        eliminate factor'
        go (factor' + 2)

    findNext !num = BitSet.read primes num >>= \case
      True -> pure num
      False -> findNext (num + 2)

    eliminate !factor = do
      let
        loop num
          | num < sieveSize = do
            BitSet.write primes num False
            loop (num + 2 * factor)
          | otherwise = pure ()
      loop (factor * factor)

  go 3
  sieveSet <- BitSet.unsafeFreeze primes

  pure SieveResult
    { sieveResultSize = sieveSize
    , sieveResultNumPrimes = if sieveSize < 2
        then 0
        else 1 + length (filter odd (drop 2 (BitSet.toList sieveSet)))
    , sieveResultPrimes =
        if sieveSize < 2
          then []
          else 2 : filter odd (drop 2 (BitSet.toList sieveSet))
    }
