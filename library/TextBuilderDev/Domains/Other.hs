module TextBuilderDev.Domains.Other where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import TextBuilderDev.Base
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Domains.Unicode
import TextBuilderDev.Prelude hiding (intercalate, length, null)

-- * Destructors

-- | Convert builder to string.
{-# INLINE toString #-}
toString :: TextBuilder -> String
toString = Text.unpack . toText

-- * Constructors

-- |
-- Run the builder and pack the produced text into a new builder.
--
-- Useful to have around builders that you reuse,
-- because a forced builder is much faster,
-- since it's virtually a single call to @memcopy@.
{-# INLINE force #-}
force :: TextBuilder -> TextBuilder
force = text . toText

-- | Lazy text.
{-# INLINE lazyText #-}
lazyText :: TextLazy.Text -> TextBuilder
lazyText =
  TextLazy.foldrChunks (mappend . text) mempty

-- | String.
{-# INLINE string #-}
string :: String -> TextBuilder
string =
  foldMap char

-- | Decimal representation of an integral value with thousands separated by the specified character.
--
-- >>> thousandSeparatedDecimal ',' 1234567890
-- "1,234,567,890"
--
-- >>> thousandSeparatedDecimal ',' (-1234567890)
-- "-1,234,567,890"
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: (Integral a) => Char -> a -> TextBuilder
thousandSeparatedDecimal separatorChar =
  signed (thousandSeparatedUnsignedDecimal separatorChar)

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> thousandSeparatedUnsignedDecimal ',' 1234567890
-- "1,234,567,890"
--
-- >>> thousandSeparatedUnsignedDecimal ' ' 1234567890
-- "1 234 567 890"
--
-- >>> thousandSeparatedUnsignedDecimal ',' 0
-- "0"
{-# INLINEABLE thousandSeparatedUnsignedDecimal #-}
thousandSeparatedUnsignedDecimal :: (Integral a) => Char -> a -> TextBuilder
thousandSeparatedUnsignedDecimal separatorChar =
  processRightmostDigit
  where
    processRightmostDigit value =
      case divMod value 10 of
        (value, digit) ->
          processAnotherDigit [decimalDigit digit] (1 :: Int) value
    processAnotherDigit builders index value =
      if value == 0
        then mconcat builders
        else case divMod value 10 of
          (value, digit) ->
            if mod index 3 == 0
              then
                processAnotherDigit
                  (decimalDigit digit : char separatorChar : builders)
                  (succ index)
                  value
              else
                processAnotherDigit
                  (decimalDigit digit : builders)
                  (succ index)
                  value

-- | Data size in decimal notation over amount of bytes.
--
-- >>> dataSizeInBytesInDecimal 999
-- "999B"
--
-- >>> dataSizeInBytesInDecimal 9999
-- "9.9kB"
--
-- >>> dataSizeInBytesInDecimal (-9999)
-- "-9.9kB"
--
-- >>> dataSizeInBytesInDecimal 1234567890
-- "1.2GB"
--
-- >>> dataSizeInBytesInDecimal 10000000000000000000000000000000023
-- "10,000,000,000YB"
{-# INLINEABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: (Integral a) => a -> TextBuilder
dataSizeInBytesInDecimal = signed \a ->
  if a < 1000
    then unsignedDecimal a <> "B"
    else
      if a < 1000000
        then dividedDecimal 100 a <> "kB"
        else
          if a < 1000000000
            then dividedDecimal 100000 a <> "MB"
            else
              if a < 1000000000000
                then dividedDecimal 100000000 a <> "GB"
                else
                  if a < 1000000000000000
                    then dividedDecimal 100000000000 a <> "TB"
                    else
                      if a < 1000000000000000000
                        then dividedDecimal 100000000000000 a <> "PB"
                        else
                          if a < 1000000000000000000000
                            then dividedDecimal 100000000000000000 a <> "EB"
                            else
                              if a < 1000000000000000000000000
                                then dividedDecimal 100000000000000000000 a <> "ZB"
                                else dividedDecimal 100000000000000000000000 a <> "YB"
  where
    dividedDecimal divisor n =
      let byDivisor = div n divisor
          byExtraTen = div byDivisor 10
          remainder = byDivisor - byExtraTen * 10
          separatorChar = ','
       in if remainder == 0 || byExtraTen >= 10
            then thousandSeparatedDecimal separatorChar byExtraTen
            else thousandSeparatedDecimal separatorChar byExtraTen <> "." <> decimalDigit remainder

-- | Unsigned binary number padded to the maximum amount of bits supported by the type.
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> TextBuilder
unsignedPaddedBinary a =
  padFromLeft (finiteBitSize a) '0' $ unsignedBinary a

-- | Intercalate builders.
{-# INLINE intercalate #-}
intercalate :: (Foldable f) => TextBuilder -> f TextBuilder -> TextBuilder
intercalate separator = extract . foldl' step init
  where
    init = Product2 False mempty
    step (Product2 isNotFirst builder) element =
      Product2 True
        $ if isNotFirst
          then builder <> separator <> element
          else element
    extract (Product2 _ builder) = builder

-- | Intercalate projecting values to builder.
{-# INLINE intercalateMap #-}
intercalateMap :: (Foldable f) => TextBuilder -> (a -> TextBuilder) -> f a -> TextBuilder
intercalateMap separator mapper = extract . foldl' step init
  where
    init = Nothing
    step acc element =
      Just $ case acc of
        Nothing -> mapper element
        Just acc -> acc <> separator <> mapper element
    extract = fromMaybe mempty

-- | Double with a fixed number of decimal places.
--
-- >>> fixedDouble 4 0.123456
-- "0.1235"
--
-- >>> fixedDouble 2 2.1
-- "2.10"
--
-- >>> fixedDouble (-2) 2.1
-- "2"
--
-- >>> fixedDouble 2 (-2.1)
-- "-2.10"
--
-- >>> fixedDouble 2 0
-- "0.00"
{-# INLINE fixedDouble #-}
fixedDouble ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
fixedDouble (max 0 -> decimalPlaces) =
  fromString . printf ("%." ++ show decimalPlaces ++ "f")

-- | Double multiplied by 100 with a fixed number of decimal places applied and followed by a percent-sign.
--
-- >>> doublePercent 3 0.123456
-- "12.346%"
--
-- >>> doublePercent 0 2
-- "200%"
--
-- >>> doublePercent 0 (-2)
-- "-200%"
{-# INLINE doublePercent #-}
doublePercent ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
doublePercent decimalPlaces x = fixedDouble decimalPlaces (x * 100) <> "%"

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
