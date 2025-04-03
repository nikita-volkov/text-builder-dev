-- | Higher-level definitions not dealing with the structure of builder.
module TextBuilderDev.Extras where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextLazy
import TextBuilderDev.Base
import TextBuilderDev.Prelude hiding (intercalate, length, null)

-- * Destructors

-- | Convert builder to string.
{-# INLINE toString #-}
toString :: TextBuilder -> String
toString = Text.unpack . toText

-- ** Output IO

-- | Put builder, to stdout.
putToStdOut :: TextBuilder -> IO ()
putToStdOut = Text.hPutStr stdout . toText

-- | Put builder, to stderr.
putToStdErr :: TextBuilder -> IO ()
putToStdErr = Text.hPutStr stderr . toText

-- | Put builder, followed by a line, to stdout.
putLnToStdOut :: TextBuilder -> IO ()
putLnToStdOut = Text.hPutStrLn stdout . toText

-- | Put builder, followed by a line, to stderr.
putLnToStdErr :: TextBuilder -> IO ()
putLnToStdErr = Text.hPutStrLn stderr . toText

-- * Constructors

-- |
-- Run the builder and pack the produced text into a new builder.
--
-- Useful to have around builders that you reuse,
-- because a forced builder is much faster,
-- since it's virtually a single call @memcopy@.
{-# INLINE force #-}
force :: TextBuilder -> TextBuilder
force = text . toText

-- | Unicode character.
{-# INLINE char #-}
char :: Char -> TextBuilder
char = unicodeCodePoint . ord

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

signed :: (Ord a, Num a) => (a -> TextBuilder) -> a -> TextBuilder
signed onUnsigned i =
  if i >= 0
    then onUnsigned i
    else unicodeCodePoint 45 <> onUnsigned (negate i)

-- | Decimal representation of an integral value.
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

-- | Decimal representation of an unsigned integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> unsignedDecimal 123456
-- "123456"
--
-- >>> unsignedDecimal 0
-- "0"
{-# INLINEABLE unsignedDecimal #-}
unsignedDecimal :: (Integral a) => a -> TextBuilder
unsignedDecimal = string . ($ "") . showInt

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

-- | Unsigned binary number.
{-# INLINE unsignedBinary #-}
unsignedBinary :: (Integral a) => a -> TextBuilder
unsignedBinary = string . ($ "") . showBin

-- | Unsigned binary number padded to the maximum amount of bits supported by the type.
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> TextBuilder
unsignedPaddedBinary a =
  padFromLeft (finiteBitSize a) '0' $ unsignedBinary a

-- | Hexadecimal representation of an integral value.
{-# INLINE hexadecimal #-}
hexadecimal :: (Integral a) => a -> TextBuilder
hexadecimal = signed unsignedHexadecimal

-- | Unsigned hexadecimal representation of an integral value.
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: (Integral a) => a -> TextBuilder
unsignedHexadecimal = string . ($ "") . showHex

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

-- | Pad a builder from the left side to the specified length with the specified character.
--
-- >>> padFromLeft 5 '0' "123"
-- "00123"
--
-- >>> padFromLeft 5 '0' "123456"
-- "123456"
{-# INLINEABLE padFromLeft #-}
padFromLeft :: Int -> Char -> TextBuilder -> TextBuilder
padFromLeft paddedLength paddingChar =
  textPaddedFromLeft paddedLength paddingChar . toText

-- | Pad a builder from the right side to the specified length with the specified character.
--
-- >>> padFromRight 5 '0' "123"
-- "12300"
--
-- >>> padFromRight 5 '0' "123456"
-- "123456"
{-# INLINEABLE padFromRight #-}
padFromRight :: Int -> Char -> TextBuilder -> TextBuilder
padFromRight paddedLength paddingChar =
  textPaddedFromRight paddedLength paddingChar . toText

-- | Pad a text from the left side to the specified length with the specified character.
--
-- >>> textPaddedFromLeft 5 '0' "123"
-- "00123"
--
-- >>> textPaddedFromLeft 5 '0' "123456"
-- "123456"
{-# INLINEABLE textPaddedFromLeft #-}
textPaddedFromLeft :: Int -> Char -> Text -> TextBuilder
textPaddedFromLeft paddedLength paddingChar input =
  let actualLength = Text.length input
   in if paddedLength <= actualLength
        then text input
        else foldMap char (replicate (paddedLength - actualLength) paddingChar) <> text input

-- | Pad a text from the right side to the specified length with the specified character.
--
-- >>> textPaddedFromRight 5 '0' "123"
-- "12300"
--
-- >>> textPaddedFromRight 5 '0' "123456"
-- "123456"
{-# INLINEABLE textPaddedFromRight #-}
textPaddedFromRight :: Int -> Char -> Text -> TextBuilder
textPaddedFromRight paddedLength paddingChar inputText =
  let actualLength = Text.length inputText
   in if paddedLength <= actualLength
        then text inputText
        else text inputText <> foldMap char (replicate (paddedLength - actualLength) paddingChar)

-- | UTC time in ISO8601 format.
--
-- >>> utcTimeInIso8601 (read "2021-11-24 12:11:02 UTC")
-- "2021-11-24T12:11:02Z"
utcTimeInIso8601 :: UTCTime -> TextBuilder
utcTimeInIso8601 UTCTime {..} =
  let (year, month, day) = toGregorian utctDay
      daySeconds = round utctDayTime
      (dayMinutes, second) = divMod daySeconds 60
      (hour, minute) = divMod dayMinutes 60
   in utcTimestampInIso8601 (fromIntegral year) month day hour minute second

-- |
-- General template for formatting date values according to the ISO8601 standard.
-- The format is the following:
--
-- Integrations with various time-libraries can be easily derived from that.
--
-- >>> utcTimestampInIso8601 2021 11 24 12 11 02
-- "2021-11-24T12:11:02Z"
utcTimestampInIso8601 ::
  -- | Year.
  Int ->
  -- | Month.
  Int ->
  -- | Day.
  Int ->
  -- | Hour.
  Int ->
  -- | Minute.
  Int ->
  -- | Second.
  Int ->
  TextBuilder
utcTimestampInIso8601 y mo d h mi s =
  mconcat
    [ fixedUnsignedDecimal 4 y,
      "-",
      fixedUnsignedDecimal 2 mo,
      "-",
      fixedUnsignedDecimal 2 d,
      "T",
      fixedUnsignedDecimal 2 h,
      ":",
      fixedUnsignedDecimal 2 mi,
      ":",
      fixedUnsignedDecimal 2 s,
      "Z"
    ]

-- |
-- Time interval in seconds.
--
-- The format is the following:
--
-- > DD:HH:MM:SS
--
-- Directly applicable to 'DiffTime' and 'NominalDiffTime'.
{-# INLINEABLE intervalInSeconds #-}
intervalInSeconds :: (RealFrac seconds) => seconds -> TextBuilder
intervalInSeconds interval = flip evalState (round interval :: Int) $ do
  seconds <- state (swap . flip divMod 60)
  minutes <- state (swap . flip divMod 60)
  hours <- state (swap . flip divMod 24)
  days <- get
  return
    ( mconcat
        [ padFromLeft 2 '0' (decimal days),
          ":",
          padFromLeft 2 '0' (decimal hours),
          ":",
          padFromLeft 2 '0' (decimal minutes),
          ":",
          padFromLeft 2 '0' (decimal seconds)
        ]
    )

-- | DiffTime in a compact decimal format based on 'picosecondsCompact'.
diffTimeCompact :: DiffTime -> TextBuilder
diffTimeCompact = picosecondsCompact . diffTimeToPicoseconds

-- | Amount of picoseconds represented in a compact decimal format using suffixes.
--
-- E.g., the following is @1_230_000_000@ picoseconds or 1.23 milliseconds or 1230 microseconds:
--
-- > 1230us
picosecondsCompact :: Integer -> TextBuilder
picosecondsCompact x =
  attemptOr 1_000_000_000_000 "s"
    $ attemptOr 1_000_000_000 "ms"
    $ attemptOr 1_000_000 "us"
    $ attemptOr 1_000 "ns"
    $ decimal x
    <> "ps"
  where
    attemptOr factor suffix alternative =
      if x == divided * factor
        then decimal divided <> suffix
        else alternative
      where
        divided = div x factor

-- | Double with a fixed number of decimal places.
{-# INLINE fixedDouble #-}
fixedDouble ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
fixedDouble decimalPlaces = fromString . printf ("%." ++ show decimalPlaces ++ "f")

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
