module TextBuilderDev.Domains.Digits where

import qualified Data.Text.Array as TextArray
import TextBuilderDev.Base
import TextBuilderDev.Domains.Unicode
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
decimal = signed unsignedDecimal

-- | Signed octal representation of an integral value.
--
-- >>> octal 123456
-- "361100"
--
-- >>> octal (-123456)
-- "-361100"
{-# INLINE octal #-}
octal :: (Integral a) => a -> TextBuilder
octal = signed unsignedOctal

-- | Hexadecimal representation of an integral value.
{-# INLINE hexadecimal #-}
hexadecimal :: (Integral a) => a -> TextBuilder
hexadecimal = signed unsignedHexadecimal

-- * Unsigned Numbers

-- | Render a number in the given radix.
{-# INLINE digitsByRadix #-}
digitsByRadix :: (Integral a) => a -> (a -> a) -> a -> TextBuilder
digitsByRadix radix digitCodepoint =
  go 0 []
  where
    go !offset !digits x = case divMod x radix of
      (next, digit) ->
        if next <= 0
          then finish (succ offset) (digit : digits)
          else go (succ offset) (digit : digits) next

    finish size digits =
      TextBuilder size action
      where
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
-- >>> unsignedBinary 0
-- "0"
--
-- >>> unsignedBinary 1
-- "1"
--
-- >>> unsignedBinary 2
-- "10"
--
-- >>> unsignedBinary 3
-- "11"
{-# INLINE unsignedBinary #-}
unsignedBinary :: (Integral a) => a -> TextBuilder
unsignedBinary =
  digitsByRadix 2 (+ 48)

-- | Unsigned octal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
--
-- >>> unsignedOctal 7
-- "7"
--
-- >>> unsignedOctal 9
-- "11"
--
-- >>> unsignedOctal 16
-- "20"
{-# INLINE unsignedOctal #-}
unsignedOctal :: (Integral a) => a -> TextBuilder
unsignedOctal =
  digitsByRadix 8 (+ 48)

-- | Unsigned decimal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> unsignedDecimal 123456
-- "123456"
--
-- >>> unsignedDecimal 0
-- "0"
{-# INLINE unsignedDecimal #-}
unsignedDecimal :: (Integral a) => a -> TextBuilder
unsignedDecimal =
  digitsByRadix 10 (+ 48)

-- | Unsigned hexadecimal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> unsignedHexadecimal 123456
-- "1e240"
--
-- >>> unsignedHexadecimal 0
-- "0"
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: (Integral a) => a -> TextBuilder
unsignedHexadecimal =
  digitsByRadix 16 (\digit -> if digit <= 9 then digit + 48 else digit + 87)
