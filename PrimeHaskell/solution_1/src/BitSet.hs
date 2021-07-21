{-# LANGUAGE NumericUnderscores #-}
module BitSet
  ( MBitSet
  , new
  , read
  , write
  , unsafeRead
  , unsafeWrite
  , BitSet
  , unsafeFreeze
  , (!)
  , toList
  ) where

import Prelude hiding (read)

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Word (Word64)
import Data.Bits (Bits (shiftR, complement, (.&.), testBit, setBit, clearBit))

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data BitSet = BitSet !Int !(VU.Vector Word64)

data MBitSet s = MBitSet !Int !(VUM.MVector s Word64)

new :: PrimMonad m => Int -> Bool -> m (MBitSet (PrimState m))
new size initial = do
  let
    initialValue
      | initial = complement 0
      | otherwise = 0
    roundedSize = (size + 63) `div` 64
  array <- VUM.replicate roundedSize initialValue
  pure (MBitSet size array)


read :: PrimMonad m => MBitSet (PrimState m) -> Int -> m Bool
{-# INLINE read #-}
read set@(MBitSet size _) index
  | index >= 0 && index < size = unsafeRead set index
  | otherwise = error "out of bounds"

write :: PrimMonad m => MBitSet (PrimState m) -> Int -> Bool -> m ()
{-# INLINE write #-}
write set@(MBitSet size _) index value
  | index >= 0 && index < size = unsafeWrite set index value
  | otherwise = error "out of bounds"

unsafeRead :: PrimMonad m => MBitSet (PrimState m) -> Int -> m Bool
{-# INLINE unsafeRead #-}
unsafeRead (MBitSet _ array) index = do
  let (wordIndex, bitIndex) = decomposeIndex index
  word <- VUM.unsafeRead array wordIndex
  pure $! testBit word bitIndex

unsafeWrite :: PrimMonad m => MBitSet (PrimState m) -> Int -> Bool -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite (MBitSet _ array) index value = do
  let (wordIndex, bitIndex) = decomposeIndex index
  word <- VUM.unsafeRead array wordIndex
  let
    word'
      | value = setBit word bitIndex
      | otherwise = clearBit word bitIndex
  VUM.unsafeWrite array wordIndex word'

decomposeIndex :: Int -> (Int, Int)
{-# INLINE decomposeIndex #-}
decomposeIndex index =
  let
    wordIndex = index `shiftR` 6
    bitIndex = index .&. 0x3F
  in
    (wordIndex, bitIndex)


unsafeFreeze :: PrimMonad m => MBitSet (PrimState m) -> m BitSet
unsafeFreeze (MBitSet size array) = do
  frozen <- VU.unsafeFreeze array
  pure (BitSet size frozen)


(!) :: BitSet -> Int -> Bool
{-# INLINE (!) #-}
(!) (BitSet size array) index
  | index >= 0 && index < size =
      let
        (wordIndex, bitIndex) = decomposeIndex index
        word = array VU.! wordIndex
      in
        testBit word bitIndex
  | otherwise = error "out of bounds"

-- | Return a list of the bits that are set
toList :: BitSet -> [Int]
toList set@(BitSet size _) = filter (set !) [0..size - 1]
