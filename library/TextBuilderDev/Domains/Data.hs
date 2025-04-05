module TextBuilderDev.Domains.Data where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text.Array as TextArray
import TextBuilderDev.Core
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Other
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Prelude hiding (intercalate, length, null)

-- | Hexadecimal readable representation of binary data.
--
-- >>> hexData "Hello"
-- "4865 6c6c 6f"
{-# INLINE hexData #-}
hexData :: ByteString -> TextBuilder
hexData =
  intercalate " "
    . fmap mconcat
    . Split.chunksOf 2
    . fmap byte
    . ByteString.unpack
  where
    byte =
      padFromLeft 2 '0' . unsignedHexadecimal

-- | Bits of a statically sized value.
-- If it's an integer, the sign is reflected in the bits.
--
-- >>> bits @Int 0
-- "0"
--
-- >>> bits @Int 4
-- "100"
--
-- >>> bits @Int8 (-4)
-- "11111100"
{-# INLINE bits #-}
bits :: (FiniteBits a) => a -> TextBuilder
bits val =
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
-- If it's an integer, the sign is reflected in the bits.
--
-- >>> paddedBits @Int8 0
-- "00000000"
--
-- >>> paddedBits @Int8 4
-- "00000100"
--
-- >>> paddedBits @Int16 4
-- "0000000000000100"
--
-- >>> paddedBits @Int16 (-4)
-- "1111111111111100"
paddedBits :: (FiniteBits a) => a -> TextBuilder
paddedBits val =
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
