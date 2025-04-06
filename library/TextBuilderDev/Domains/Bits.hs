module TextBuilderDev.Domains.Bits where

import qualified Data.Text.Array as TextArray
import TextBuilderCore
import TextBuilderDev.Prelude hiding (intercalate, length, null)

-- | Bits of a statically sized value.
-- If it's a negatable integer, the sign is reflected in the bits.
--
-- >>> finiteBits @Int 0
-- "0"
--
-- >>> finiteBits @Int 4
-- "100"
--
-- >>> finiteBits @Int8 (-4)
-- "11111100"
{-# INLINE finiteBits #-}
finiteBits :: (FiniteBits a) => a -> TextBuilder
finiteBits val =
  let size = max 1 (finiteBitSize val - countLeadingZeros val)
   in TextBuilder size \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex
                    $ if testBit val 0 then 49 else 48
                  go (unsafeShiftR val 1) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)

-- | Bits of a statically sized value padded from the left according to the size.
-- If it's a negatable integer, the sign is reflected in the bits.
--
-- >>> paddedFiniteBits @Int8 0
-- "00000000"
--
-- >>> paddedFiniteBits @Int8 4
-- "00000100"
--
-- >>> paddedFiniteBits @Int16 4
-- "0000000000000100"
--
-- >>> paddedFiniteBits @Int16 (-4)
-- "1111111111111100"
{-# INLINE paddedFiniteBits #-}
paddedFiniteBits :: (FiniteBits a) => a -> TextBuilder
paddedFiniteBits val =
  let size = finiteBitSize val
   in TextBuilder size \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex $ if testBit val 0 then 49 else 48
                  go (unsafeShiftR val 1) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)
