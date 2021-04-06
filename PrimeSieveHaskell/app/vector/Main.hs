{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified System.Clock as Clock

import Lib

main :: IO ()
main = do
  let durationCutoff = Clock.TimeSpec { Clock.sec = 5, Clock.nsec = 0 }
  let sieveSize = 1_000_000

  runBenchmark sieveSize durationCutoff sieve

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
