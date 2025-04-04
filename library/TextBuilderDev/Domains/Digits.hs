module TextBuilderDev.Domains.Digits where

import qualified Data.List as List
import qualified Data.Text.Array as TextArray
import TextBuilderDev.Base
import qualified TextBuilderDev.Domains.Digits.List as Digits.List
import TextBuilderDev.Prelude

-- | Decimal digit.
{-# INLINE decimalDigit #-}
decimalDigit :: (Integral a) => a -> TextBuilder
decimalDigit (fromIntegral -> n) =
  unicodeCodePoint (n + 48)

-- | Hexadecimal digit.
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: (Integral a) => a -> TextBuilder
hexadecimalDigit (fromIntegral -> n) =
  if n <= 9
    then unicodeCodePoint (n + 48)
    else unicodeCodePoint (n + 87)

-- * Signed Numbers

{-# INLINE signed #-}
signed :: (Ord a, Num a) => (a -> TextBuilder) -> a -> TextBuilder
signed onUnsigned i =
  if i >= 0
    then onUnsigned i
    else unicodeCodePoint 45 <> onUnsigned (negate i)

-- | Signed decimal representation of an integral value.
--
-- >>> decimal 123456
-- "123456"
--
-- >>> decimal (-123456)
-- "-123456"
--
-- >>> decimal 0
-- "0"
{-# INLINEABLE decimal #-}
decimal :: (Integral a) => a -> TextBuilder
decimal = signed decimalDigits

-- | Hexadecimal representation of an integral value.
{-# INLINE hexadecimal #-}
hexadecimal :: (Integral a) => a -> TextBuilder
hexadecimal = signed hexadecimalDigits

-- * Unsigned Numbers

-- | Render a number in the given radix.
digitsByRadix :: (Integral a) => a -> (a -> a) -> a -> TextBuilder
digitsByRadix radix digitCodepoint x =
  TextBuilder size action
  where
    size = List.length digits

    digits = Digits.List.digits radix x

    action :: TextArray.MArray s -> Int -> ST s Int
    action array =
      go digits
      where
        go digits offset = case digits of
          [] -> return offset
          (digit : digits) -> do
            TextArray.unsafeWrite array offset (fromIntegral (digitCodepoint digit))
            go digits (succ offset)

-- | Unsigned binary representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> binaryDigits 0
-- "0"
--
-- >>> binaryDigits 1
-- "1"
--
-- >>> binaryDigits 2
-- "10"
--
-- >>> binaryDigits 3
-- "11"
{-# INLINE binaryDigits #-}
binaryDigits :: (Integral a) => a -> TextBuilder
binaryDigits =
  digitsByRadix 2 (+ 48)

-- | Unsigned octal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
--
-- >>> octalDigits 7
-- "7"
--
-- >>> octalDigits 9
-- "11"
--
-- >>> octalDigits 16
-- "20"
{-# INLINE octalDigits #-}
octalDigits :: (Integral a) => a -> TextBuilder
octalDigits =
  digitsByRadix 8 (+ 48)

-- | Unsigned decimal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> decimalDigits 123456
-- "123456"
--
-- >>> decimalDigits 0
-- "0"
{-# INLINE decimalDigits #-}
decimalDigits :: (Integral a) => a -> TextBuilder
decimalDigits =
  digitsByRadix 10 (+ 48)

-- | Unsigned hexadecimal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> hexadecimalDigits 123456
-- "1e240"
--
-- >>> hexadecimalDigits 0
-- "0"
{-# INLINE hexadecimalDigits #-}
hexadecimalDigits :: (Integral a) => a -> TextBuilder
hexadecimalDigits =
  digitsByRadix 16 (\digit -> if digit <= 9 then digit + 48 else digit + 87)
