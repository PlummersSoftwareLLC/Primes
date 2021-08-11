{-# OPTIONS_GHC -O2 -fllvm #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Time.Clock.POSIX ( getPOSIXTime, POSIXTime )
import Data.Word ( Word8, Word64 )
import Data.Bits ( Bits((.|.), (.&.), shiftL, shiftR) )
import Control.Monad ( forM_, foldM_, foldM )
import Control.Monad.ST ( ST, runST )
import Data.Array.Base ( MArray(newArray), STUArray, castSTUArray,
                         unsafeRead, unsafeWrite,
                         UArray, listArray, assocs, unsafeAt )
import Data.Array.ST ( runSTUArray )

type Prime = Word64
type SieveBuffer = UArray Int Bool

cLIMIT :: Prime
cLIMIT = 1000000

cFORTIME :: POSIXTime
cFORTIME = 5

cCPUL1CACHE :: Int 
cCPUL1CACHE = 16384 -- in bytes, must be power of two

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

primesSoEUnpeeled :: Prime -> SieveBuffer
primesSoEUnpeeled limit = runSTUArray $ do -- following in ST Monad so can mutate array
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
    lstv <- unsafeRead cs lastByteIndex -- mask primes above bitlmt
    unsafeWrite cs lastByteIndex (lstv .|. (0xFE `shiftL` (bitlmt .&. 7)))
    return csb -- actual deliverable is boolean for convenience in decoding

primesSoEUnpeeledBlock :: Prime -> SieveBuffer
primesSoEUnpeeledBlock limit = runSTUArray $ do -- in ST Monad to mutate array
    let bitlmt = fromIntegral ((limit - 3) `div` 2)
    csb <- newArray (0, bitlmt) False -- boolean and Word8 views of array
    cs <- (castSTUArray :: STUArray s Int Bool ->
                             ST s (STUArray s Int Word8)) csb
    strts <- newArray (0, 7) 0 :: ST s (STUArray s Int Int)
    let lastByteIndex = bitlmt `shiftR` 3
        sqrtlmtndx = floor (sqrt (fromIntegral limit) - 3) `div` 2
    forM_ [ 0 .. sqrtlmtndx ] $ \ ndx -> do -- outer loop finding base primes
      b <- unsafeRead csb ndx
      if b then return () else do -- all cases must be covered; found one!
        let basePrime = ndx + ndx + 3
            startIndex = (basePrime * basePrime - 3) `shiftR` 1
            firstPageIndex = (startIndex `shiftR` 3) .&. (-cCPUL1CACHE)
            loopLimit = min bitlmt $ startIndex + (basePrime `shiftL` 3) - 1
        -- initialize start byte addresses...
        forM_ [ startIndex, startIndex + basePrime .. loopLimit ]
          $ \ v -> unsafeWrite strts (v .&. 7) (v `shiftR` 3)
        forM_ [firstPageIndex, firstPageIndex + cCPUL1CACHE
                                 .. lastByteIndex] $ \ pageByteIndex -> do
          let pageByteLimit =
                min lastByteIndex $ pageByteIndex + cCPUL1CACHE - 1
              lastUnrolledIndex = pageByteLimit - basePrime * 3

          forM_ [ 0 .. 7 ] $ \ i -> do -- up to eight sub culling loops...
            let mask = unsafeAt cBITMASK i
            -- up to eight sub loops by constant mask; unrolled by four;
            -- use foldM to thread the last cull value through...
            startByteIndex <- unsafeRead strts i
            nextByteIndex <- foldM (\ _ cull -> do
                -- unroll by four culls...
                v0 <- unsafeRead cs cull; unsafeWrite cs cull (v0 .|. mask)
                let c1 = cull + basePrime
                v1 <- unsafeRead cs c1; unsafeWrite cs c1 (v1 .|. mask)
                let c2 = c1 + basePrime
                v2 <- unsafeRead cs c2; unsafeWrite cs c2 (v2 .|. mask)
                let c3 = c2 + basePrime
                v3 <- unsafeRead cs c3; unsafeWrite cs c3 (v3 .|. mask)
                return $ c3 + basePrime) -- previous index returned here
              startByteIndex -- default case for no loops!
              [ startByteIndex, startByteIndex + (basePrime * 4) ..
                                                      lastUnrolledIndex ]
            -- do culls that can't be unrolled (too few)...
            succByteIndex <- foldM (\ _ cull -> do
                v <- unsafeRead cs cull; unsafeWrite cs cull (v .|. mask)
                return $ cull + basePrime)
              nextByteIndex -- in case of no loops
              [ nextByteIndex, nextByteIndex + basePrime .. pageByteLimit ]
            unsafeWrite strts i succByteIndex
    lstv <- unsafeRead cs lastByteIndex -- mask primes above bitlmt
    unsafeWrite cs lastByteIndex (lstv .|. (0xFE `shiftL` (bitlmt .&. 7)))
    return csb -- actual deliverable is boolean for convenience in decoding

listPrimes :: SieveBuffer -> [Prime]
listPrimes sb =
   sb `seq` 2 : [ fromIntegral (i + i + 3) | (i, False) <- assocs sb ]

benchMark :: String -> (Prime -> SieveBuffer) -> IO ()
benchMark label testprimefnc = do
  strttm <- getPOSIXTime
  let loop _ [] = error "Should never get here!!!"
      loop passes (hd : rst) = do
        let cmpstsBuffer = testprimefnc hd
        now <- cmpstsBuffer `seq` getPOSIXTime -- force immediate execution
        let duration = now - strttm
        if duration < cFORTIME then passes `seq` loop (passes + 1) rst else
          let count = length $ listPrimes cmpstsBuffer in
          if count == cEXPECTED then
            putStrLn $ "GordonBGood_" ++ label ++ ";"
                      ++ show passes ++ ";" ++ show (realToFrac duration)
                      ++ ";1;algorithm=base,faithful=yes,bits=1"
          else putStrLn $ "Invalid result:  " ++ show count ++ " primes." ++ show passes
  loop 0 (repeat cLIMIT)

main :: IO ()
main = do
  forM_ [ ("unpeeled", primesSoEUnpeeled)
        , ("unpeeled_block", primesSoEUnpeeledBlock)
        ] $ uncurry benchMark

