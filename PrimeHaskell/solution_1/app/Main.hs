{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Foldable (traverse_)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified System.Clock as Clock

import Lib
import qualified BitSet

main :: IO ()
main = do
  let durationCutoff = Clock.TimeSpec { Clock.sec = 5, Clock.nsec = 0 }
  let sieveSize = 1_000_000

  let
    mkBench keyword bits run = Benchmark
        { benchmarkLabel = "fatho/" <> keyword
        , benchmarkCutoff = durationCutoff
        , benchmarkRun = run sieveSize
        , benchmarkThreads = 1
        , benchmarkFaithful = False
        , benchmarkAlgorithm = "base"
        , benchmarkBits = bits
        }

    benches =
      [ mkBench "bitset" 1 sieveBitSet
      , mkBench "bitset_unchecked" 1 sieveBitSetUnchecked
      , mkBench "vector" 8 sieveVector
      , mkBench "vector_unchecked" 8 sieveVectorUnchecked
      ]

  traverse_ runBenchmark benches

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

sieveBitSetUnchecked :: Int -> IO SieveResult
{-# NOINLINE sieveBitSetUnchecked #-}
sieveBitSetUnchecked sieveSize = do
  let q = floor $ sqrt @Double $ realToFrac sieveSize
  primes <- BitSet.new sieveSize True

  let
    go !factor
      | factor > q = pure ()
      | otherwise = do
        factor' <- findNext factor
        eliminate factor'
        go (factor' + 2)

    findNext !num = BitSet.unsafeRead primes num >>= \case
      True -> pure num
      False -> findNext (num + 2)

    eliminate !factor = do
      let
        loop num
          | num < sieveSize = do
            BitSet.unsafeWrite primes num False
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

sieveVector :: Int -> IO SieveResult
{-# NOINLINE sieveVector #-}
sieveVector sieveSize = do
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

sieveVectorUnchecked :: Int -> IO SieveResult
{-# NOINLINE sieveVectorUnchecked #-}
sieveVectorUnchecked sieveSize = do
  let q = floor $ sqrt @Double $ realToFrac sieveSize
  primes <- VUM.replicate sieveSize True

  let
    go !factor
      | factor > q = pure ()
      | otherwise = do
        factor' <- findNext factor
        eliminate factor'
        go (factor' + 2)

    findNext !num = VUM.unsafeRead primes num >>= \case
      True -> pure num
      False -> findNext (num + 2)

    eliminate !factor = do
      let
        loop num
          | num < sieveSize = do
            VUM.unsafeWrite primes num False
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
