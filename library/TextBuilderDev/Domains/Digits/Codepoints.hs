module TextBuilderDev.Domains.Digits.Codepoints where

import TextBuilderDev.Prelude

{-# INLINE octalDigit #-}
octalDigit :: Word8 -> Word8
octalDigit a = a .&. 7 + 48

-- | Extract the first 4 bits and convert them to a Unicode codepoint of a hexadecimal digit.
-- The result is a character in the range of 0-9 or a-f.
--
-- >>> chr (fromIntegral (hexDigit 0))
-- '0'
--
-- >>> chr (fromIntegral (hexDigit 10))
-- 'a'
--
-- >>> chr (fromIntegral (hexDigit 15))
-- 'f'
--
-- The overflow is ignored, so the result is always in the range of 0-15:
-- >>> chr (fromIntegral (hexDigit 16))
-- '0'
{-# INLINE hexDigit #-}
hexDigit :: Word8 -> Word8
hexDigit a = case a .&. 15 of
  a ->
    if a < 10
      then a + 48
      else a + 87
