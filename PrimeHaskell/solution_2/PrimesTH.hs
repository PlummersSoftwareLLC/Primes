-- Template haskell Module for Sieve of Eratosthenes algorithm...
--   implements extree loop unrolling and dense culling...
--   Note:  `case` is often used to force strictness rather than as a branch!

{-# OPTIONS_GHC -O2 -fllvm #-}
{-# LANGUAGE CPP, TemplateHaskell, MagicHash, UnboxedTuples #-}

module PrimesTH ( makeLoopFuncArr, cDENSETHRESHOLD, makeDenseFuncArr ) where

import Language.Haskell.TH ( litE, intPrimL, wordPrimL, ExpQ )

import Data.Bits ( Bits( shiftR, shiftL, (.&.), (.|.) ) )
import GHC.Exts ( Int(I#), Int#, Word#, Addr#, State#(..),
  (+#), (-#), (*#), (<#), or#, orI#, andI#, int2Word#,
  uncheckedIShiftRL#, uncheckedShiftL#, plusAddr#, minusAddr#, ltAddr#,
  readWord8OffAddr#, writeWord8OffAddr#,
  readWord64OffAddr#, writeWord64OffAddr# )
#ifdef AVX2
import GHC.Exts ( Word64X4#, packWord64X4#, unpackWord64X4#,
                  readWord64X4OffAddr#, writeWord64X4OffAddr# )
#else
#ifdef AVX
import GHC.Exts ( Word64X4#, packWord64X2#, unpackWord64X2#,
                  readWord64X2OffAddr#, writeWord64X2OffAddr# )
#endif
#endif

cNUMUNROLLEDCASES :: Int
cNUMUNROLLEDCASES = 32

cDENSETHRESHOLD :: Int
cDENSETHRESHOLD = 129

-- following code is for extreme unrolling function array builder macro...

cullBit :: Int -> Int -> ExpQ -- cull bit modadically by primitives
cullBit bi n = -- cull (set one) bit index `bi` for case selection `n`...
  let rq = case bi of 0 -> [| 0# |] -- name the register `r`, 0th none...
                      1 -> [| r1# |]
                      2 -> [| r2# |]
                      3 -> [| r3# |]
                      4 -> [| r4# |]
                      5 -> [| r5# |]
                      6 -> [| r6# |]
                      7 -> [| r7# |]
      stp8 = ((n `shiftR` 2) .|. 1) .&. 7 -- extract the step value
      msk = (n + bi * stp8) .&. 7 -- extract the mask index
      mq = litE $ wordPrimL $ 1 `shiftL` msk in
  [|  \s0# -> case readWord8OffAddr# ai# $rq s0# of -- read byte value
      (# s1#, v# #) ->
        let nv# = v# `or#` $mq in -- modify value
        case writeWord8OffAddr# ai# $rq nv# s1# of
        s2# -> s2# |] -- after writing new value

closeCulls :: Int -> ExpQ -- cull remaining < one loop span monadically...
closeCulls n = -- customized by case number `n`...
  let loop i = -- a nested monadic conditional loop...
        if i >= 7 then [| \ s# -> (# s#, () #) |] else -- maximum 7 culls!
        let rq = case i of 0 -> [| 0# |] -- name the register `r`, 0th none...
                           1 -> [| r1# |]
                           2 -> [| r2# |]
                           3 -> [| r3# |]
                           4 -> [| r4# |]
                           5 -> [| r5# |]
                           6 -> [| r6# |]
                           7 -> [| r7# |]
            rlq = [| rlmt# |] in -- `rlmt#` is definied in outer calling macro
        [| \ s# -> case $rq <# $rlq of -- tests extreme limit
              0# -> (# s#, () #) -- finished
              _ -> case $(cullBit i n) s# of -- otherwise loop
                so# -> $(loop (i + 1)) so# |] in loop 0

unrollCase :: Int -> ExpQ -- unroll the eight culling operations monadically...
unrollCase n =
  let loop i = if i > 7 then [| \s# -> (# s#, () #) |] else
               [| \ s# -> case $(cullBit i n) s# of -- nested monadic operations
                    so# -> $(loop (i + 1)) so# |] in loop 0

makeUnrolledFunc :: Int -> ExpQ -- make a function taking buffer address/size
makeUnrolledFunc n = -- with starting bit index and step size...
  [| \ ba# sz# bi0# stpi# -> -- first initialize all the register offsets...
    let r0# = bi0# `uncheckedIShiftRL#` 3#
        r1# = ((bi0# +# stpi#) `uncheckedIShiftRL#` 3#) -# r0#
        r2# = ((bi0# +# (stpi# *# 2#)) `uncheckedIShiftRL#` 3#) -# r0#
        r3# = ((bi0# +# (stpi# *# 3#)) `uncheckedIShiftRL#` 3#) -# r0#
        r4# = ((bi0# +# (stpi# *# 4#)) `uncheckedIShiftRL#` 3#) -# r0#
        r5# = ((bi0# +# (stpi# *# 5#)) `uncheckedIShiftRL#` 3#) -# r0#
        r6# = ((bi0# +# (stpi# *# 6#)) `uncheckedIShiftRL#` 3#) -# r0#
        r7# = ((bi0# +# (stpi# *# 7#)) `uncheckedIShiftRL#` 3#) -# r0#
        a0# = ba# `plusAddr#` r0# -- first byte address
        lmt# = ba# `plusAddr#` sz#
        almt# = lmt# `plusAddr#` (0# -# r7#)
        loopa# ai# si# = -- looping monadically up to limit
          case ai# `ltAddr#` almt# of
          0# ->
            let rlmt# = lmt# `minusAddr#` ai#
            in $(closeCulls n) si# -- limit reached, temination code
          _ ->
            case $(unrollCase n) si# of
            (# so#, _ #) -> loopa# (ai# `plusAddr#` stpi#) so#
    in ST $ \ s# -> loopa# a0# s# |] -- do it monadically from first address

makeUnrolledFuncList :: () -> ExpQ
makeUnrolledFuncList() =
  let loop i = if i >= cNUMUNROLLEDCASES then [| [] |] else
               [| $(makeUnrolledFunc i) : $(loop (i + 1)) |] in loop 0

makeLoopFuncArr :: () -> ExpQ
makeLoopFuncArr() =
  [| listArray (0 :: Int, cNUMUNROLLEDCASES - 1) $(makeUnrolledFuncList()) |]


-- following code is for dense culling function array builder macro...

cWORDSIZE :: Int
#ifdef AVX2
cWORDSIZE = 32 -- number of bytes per word

unrollDensePattern :: Integer -> ExpQ
unrollDensePattern n =
  let stp = n + n + 3
      lmt = stp * fromIntegral cWORDSIZE * 8 -- in bits
      loop bi wi =
        if bi >= lmt then [| \ s# -> (# s#, () #) |] else
        let nbi = bi + stp
            nwi = bi `shiftR` 8
            mskq = litE $ wordPrimL $ 1 `shiftL` (fromIntegral bi .&. 63) in
        if nwi > wi && (nbi `shiftR` 8) - nwi > 0 then
          let wiq = litE $ intPrimL $ nwi in
          case (bi `shiftR` 6) .&. 3 of
            0 -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w0# `or#` $mskq of
                    nw# -> case packWord64X4# (# nw#, w1#, w2#, w3# #) of
                      nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                        so# -> $(loop nbi nwi) so# |]
            1 -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w1# `or#` $mskq of
                    nw# -> case packWord64X4# (# w0#, nw#, w2#, w3# #) of
                      nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                        so# -> $(loop nbi nwi) so# |]
            2 -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w2# `or#` $mskq of
                    nw# -> case packWord64X4# (# w0#, w1#, nw#, w3# #) of
                      nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                        so# -> $(loop nbi nwi) so# |]
            _ -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w3# `or#` $mskq of
                    nw# -> case packWord64X4# (# w0#, w1#, w2#, nw# #) of
                      nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                        so# -> $(loop nbi nwi) so# |]
        else if nwi > wi then
          let wiq = litE $ intPrimL nwi in          
          case (bi `shiftR` 6) .&. 3 of
            0 -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w0# `or#` $mskq of
                    nw# -> $(loop nbi nwi) (# s#, nw#, w1#, w2#, w3# #) |]
            1 -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w1# `or#` $mskq of
                    nw# -> $(loop nbi nwi) (# s#, w0#, nw#, w2#, w3# #) |]
            2 -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w2# `or#` $mskq of
                    nw# -> $(loop nbi nwi) (# s#, w0#, w1#, nw#, w3# #) |]
            _ -> [| \ s# ->
              case readWord64X4OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X4# v# of
                  (# w0#, w1#, w2#, w3# #) -> case w3# `or#` $mskq of
                    nw# -> $(loop nbi nwi) (# s#, w0#, w1#, w2#, nw# #) |]
        else if nbi `shiftR` 8 > wi then
          let wiq = litE $ intPrimL wi in
          case (bi `shiftR` 6) .&. 3 of
            0 -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
              case w0# `or#` $mskq of
                nw# -> case packWord64X4# (# nw#, w1#, w2#, w3# #) of
                  nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi wi) so# |]
            1 -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
              case w1# `or#` $mskq of
                nw# -> case packWord64X4# (# w0#, nw#, w2#, w3# #) of
                  nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi wi) so# |]
            2 -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
              case w2# `or#` $mskq of
                nw# -> case packWord64X4# (# w0#, w1#, nw#, w3# #) of
                  nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi wi) so# |]
            _ -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
              case w3# `or#` $mskq of
                nw# -> case packWord64X4# (# w0#, w1#, w2#, nw# #) of
                  nv# -> case writeWord64X4OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi wi) so# |]
        else case (bi `shiftR` 6) .&. 3 of
          0 -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
                case w0# `or#` $mskq of
                  nw# -> $(loop nbi wi) (# s#, nw#, w1#, w2#, w3# #) |]
          1 -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
                case w1# `or#` $mskq of
                  nw# -> $(loop nbi wi) (# s#, w0#, nw#, w2#, w3# #) |]
          2 -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
                case w2# `or#` $mskq of
                  nw# -> $(loop nbi wi) (# s#, w0#, w1#, nw#, w3# #) |]
          _ -> [| \ (# s#, w0#, w1#, w2#, w3# #) ->
                case w3# `or#` $mskq of
                  nw# -> $(loop nbi wi) (# s#, w0#, w1#, w2#, nw# #) |]
  in loop 0 (-1) -- pre-conditioning guarantees first bit index is zero!
#else
#ifdef AVX
cWORDSIZE = 16 -- number of bytes per word

unrollDensePattern :: Integer -> ExpQ
unrollDensePattern n =
  let stp = n + n + 3
      lmt = stp * fromIntegral cWORDSIZE * 8 -- in bits
      loop bi wi =
        if bi >= lmt then [| \ s# -> (# s#, () #) |] else
        let nbi = bi + stp
            nwi = bi `shiftR` 7
            mskq = litE $ wordPrimL $ 1 `shiftL` (fromIntegral bi .&. 63) in
        if nwi > wi && (nbi `shiftR` 7) - nwi > 0 then
          let wiq = litE $ intPrimL $ nwi in
          if (bi `shiftR` 6) .&. 1 == 0 then
            [| \ s# ->
              case readWord64X2OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X2# v# of
                  (# w0#, w1# #) -> case w0# `or#` $mskq of
                    nw# -> case packWord64X2# (# nw#, w1# #) of
                      nv# -> case writeWord64X2OffAddr# ai# $wiq nv# s1# of
                        so# -> $(loop nbi nwi) so# |]
          else
            [| \ s# ->
              case readWord64X2OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X2# v# of
                  (# w0#, w1# #) -> case w1# `or#` $mskq of
                    nw# -> case packWord64X2# (# w0#, nw# #) of
                      nv# -> case writeWord64X2OffAddr# ai# $wiq nv# s1# of
                        so# -> $(loop nbi nwi) so# |]
        else if nwi > wi then
          let wiq = litE $ intPrimL nwi in          
          if (bi `shiftR` 6) .&. 1 == 0 then
            [| \ s# ->
              case readWord64X2OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X2# v# of
                  (# w0#, w1# #) -> case w0# `or#` $mskq of
                    nw# -> $(loop nbi nwi) (# s#, nw#, w1# #) |]
          else
            [| \ s# ->
              case readWord64X2OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case unpackWord64X2# v# of
                  (# w0#, w1# #) -> case w1# `or#` $mskq of
                    nw# -> $(loop nbi nwi) (# s#, w0#, nw# #) |]
        else if nbi `shiftR` 7 > wi then
          let wiq = litE $ intPrimL wi in
          if (bi `shiftR` 6) .&. 1 == 0 then
            [| \ (# s#, w0#, w1# #) ->
              case w0# `or#` $mskq of
                nw# -> case packWord64X2# (# nw#, w1# #) of
                  nv# -> case writeWord64X2OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi wi) so# |]
          else
            [| \ (# s#, w0#, w1# #) ->
              case w1# `or#` $mskq of
                nw# -> case packWord64X2# (# w0#, nw# #) of
                  nv# -> case writeWord64X2OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi wi) so# |]
        else
          if (bi `shiftR` 6) .&. 1 == 0 then
            [| \ (# s#, w0#, w1# #) ->
                case w0# `or#` $mskq of
                  nw# -> $(loop nbi wi) (# s#, nw#, w1# #) |]
          else
            [| \ (# s#, w0#, w1# #) ->
                case w1# `or#` $mskq of
                  nw# -> $(loop nbi wi) (# s#, w0#, nw# #) |]
  in loop 0 (-1) -- pre-conditioning guarantees first bit index is zero!
#else
cWORDSIZE = 8 -- number of bytes per word

unrollDensePattern :: Integer -> ExpQ
unrollDensePattern n =
  let stp = n + n + 3
      lmt = stp * 64
      loop bi wi =
        if bi >= lmt then [| \ s# -> (# s#, () #) |] else
        let nbi = bi + stp
            nwi = bi `shiftR` 6
            mskq = litE $ wordPrimL $ 1 `shiftL` (fromIntegral bi .&. 63) in
        if nwi > wi && (nbi `shiftR` 6) - nwi > 0 then
          let wiq = litE $ intPrimL nwi in
          [| \ s# -> case readWord64OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case v# `or#` $mskq of
                  nv# -> case writeWord64OffAddr# ai# $wiq nv# s1# of
                    so# -> $(loop nbi nwi) so# |]
        else if nwi > wi then
          let wiq = litE $ intPrimL nwi in
          [| \ s# -> case readWord64OffAddr# ai# $wiq s# of
                (# s1#, v# #) -> case v# `or#` $mskq of
                    nv# -> $(loop nbi nwi) (# s1#, nv# #) |]
        else if nbi `shiftR` 6 > wi then
          let wiq = litE $ intPrimL wi in
          [| \ (# s#, v# #) -> case v# `or#` $mskq of
                nv# -> case writeWord64OffAddr# ai# $wiq nv# s# of
                  so# -> $(loop nbi wi) so# |]
        else [| \ (# s#, v# #) ->
                    case v# `or#` $mskq of
                      nv# -> $(loop nbi wi) (# s#, nv# #) |]
  in loop 0 (-1) -- pre-conditioning guarantees first bit index is zero!
#endif
#endif

makeUDenseFunc :: Int -> ExpQ -- make a function taking buffer address
makeUDenseFunc n = -- and byte size with starting bit index and step size...
  let wrdszq = litE $ intPrimL $ fromIntegral cWORDSIZE in
  [| \ ba# sz# bi0# stpi# -> -- first initialize all the register offsets...
    let bitsz# = sz# *# 8# -- multiply by bits per byte!
        advi# = stpi# *# $wrdszq -- advance by step words at a time
        a0# = ba# `plusAddr#` (bi0# `uncheckedIShiftRL#` 3#)
        almt# = ba# `plusAddr#` (sz# -# (advi# -# $wrdszq)) -- loops left?
        loopa# :: Addr# -> State# s -> (# State# s, () #)
        loopa# ai# si# = -- looping monadically up to limit
          case ai# `ltAddr#` almt# of -- if limit reached, return next index...
            0# -> case (ai# `minusAddr#` ba#) *# 8# of -- six LSB's always 0's!
              newndx# ->
                let eloop# ei# se# =
                      case ei# <# bitsz# of
                      0# -> (# si#, () #)
                      _ ->
                        let wi# = ei# `uncheckedIShiftRL#` 3# in
                        case readWord8OffAddr# ba# wi# se# of
                        (# se0#, v# #) ->
                          let msk# = (int2Word# 1#) `uncheckedShiftL#`
                                                        (ei# `andI#` 7#) in
                          case v# `or#` msk# of
                            nv# -> case writeWord8OffAddr# ba# wi# nv# se0# of
                              se1# -> case ei# +# stpi# of
                                nei# -> eloop# nei# si#
                in eloop# newndx# si#
--            _ -> case $(unrollDensePattern $ fromIntegral n) si# of
            _ -> case $(unrollDensePattern $ fromIntegral n) si# of
                   (# so#, _ #) -> loopa# (ai# `plusAddr#` advi#) so#
    in ST $ \ s# -> case loopa# a0# s# of { rslt# -> rslt# } |] -- monadically!

makeDenseFuncList :: () -> ExpQ
makeDenseFuncList() =
  let loop i = if i > (cDENSETHRESHOLD - 3) `shiftR` 1 then [| [] |] else
               [| $(makeUDenseFunc i) : $(loop (i + 1)) |]
  in loop 0

makeDenseFuncArr :: () -> ExpQ
makeDenseFuncArr() =
  [| listArray (0 :: Int, (cDENSETHRESHOLD - 3) `shiftR` 1)
               $(makeDenseFuncList()) |]

