-- implementation of "unpeeling"/"striping" Sieve of Eratosthenes benchmark...

{-# OPTIONS_GHC -O2 -fllvm #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Time.Clock.POSIX ( getPOSIXTime, POSIXTime )
import Data.Word ( Word8, Word64 )
import Data.Bits ( Bits((.|.), (.&.), shiftL, shiftR) )
import Control.Monad ( forM_ )
import Control.Monad.ST ( ST, runST )
import Data.Array.Base ( MArray(newArray), STUArray, castSTUArray,
                         unsafeRead, unsafeWrite,
                         UArray, listArray, assocs, unsafeAt )
import Data.Array.ST ( runSTUArray )

type Prime = Word64

cLIMIT :: Prime
cLIMIT = 1000000

-- | Historical data for validating our results - the number of primes
-- to be found under some limit, such as 168 primes under 1000
primeCounts :: [(Prime, Int)]
primeCounts =
  [ (          10, 4         )
  , (         100, 25        )
  , (        1000, 168       )
  , (       10000, 1229      )
  , (      100000, 9592      )
  , (     1000000, 78498     )
  , (    10000000, 664579    )
  ]

cEXPECTED :: Int
cEXPECTED = maybe 0 id $ lookup cLIMIT primeCounts

cBITMASK :: UArray Int Word8
cBITMASK = listArray (0, 7) [ 1, 2, 4, 8, 16, 32, 64, 128 ]

primesSoE :: Prime -> [Prime] -- force evaluation of array
primesSoE limit = cmpsts `seq` 2 : [ fromIntegral (i + i + 3)
                                       | (i, False) <- assocs cmpsts ] where
  cmpsts = runSTUArray $ do -- following in ST Monad so can mutate array
    let bitlmt = fromIntegral ((limit - 3) `div` 2)
    csb <- newArray (0, bitlmt) False -- boolean and Word8 views of array
    cs <- (castSTUArray :: STUArray s Int Bool ->
                             ST s (STUArray s Int Word8)) csb
    let lastByteIndex = bitlmt `shiftR` 3
        sqrtlmtndx = floor (sqrt (fromIntegral limit) - 3) `div` 2
    forM_ [ 0 .. sqrtlmtndx ] $ \ ndx -> do -- outer loop finding base primes
      b <- unsafeRead csb ndx
      if b then return () else do -- all cases must be covered; found one!
        let basePrime = ndx + ndx + 3
            startIndex = (basePrime * basePrime - 3) `shiftR` 1
            loopLimit = min bitlmt $ startIndex + (basePrime `shiftL` 3) - 1
        forM_ [ startIndex, startIndex + basePrime .. loopLimit ] $ \ loop -> do
          let mask = unsafeAt cBITMASK (loop .&. 7) -- for eight culling loops
              startByteIndex = loop `shiftR` 3
          forM_ [ startByteIndex, startByteIndex + basePrime .. lastByteIndex ]
                    $ \ cull -> do -- simple sub loops by constant mask
            v <- unsafeRead cs cull; unsafeWrite cs cull (v .|. mask)
    return csb -- actual deliverable is boolean for convenience in decoding

printResults :: Double -> Int -> Int -> IO ()
printResults duration passes count = do
  if count == cEXPECTED then
    putStrLn $ "GordonBGood_unpeeled;" ++ show passes ++ ";" ++ show (1.0 *duration) ++
               ";1;algorithm=base;faithful=yes;bits=1\n"
  else putStrLn $ "Invalid result:  " ++ show count ++ " primes."

benchMark :: POSIXTime -> [Prime] -> IO ()
benchMark strttm = loop 0 where
  loop _ [] = error "Should never get here!!!"
  loop passes (hd : rst) = do
    let primes = primesSoE hd
    now <- primes `seq` getPOSIXTime -- force immediate, no deferred, execution
    let duration = now - strttm
    if duration < 5 then passes `seq` loop (passes + 1) rst
    else printResults (realToFrac duration) passes (length primes)

main :: IO ()
main = do
  startTime <- getPOSIXTime
  benchMark startTime $ repeat cLIMIT

