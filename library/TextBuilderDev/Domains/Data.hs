module TextBuilderDev.Domains.Data where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text.Array as TextArray
import TextBuilderDev.Base
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

-- | Bits of a datum with a finite size.
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
