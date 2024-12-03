{-# LANGUAGE CPP #-}

module TextBuilderDev
  ( TextBuilder,

    -- * Accessors
    buildText,
    length,
    null,

    -- ** Output IO
    putToStdOut,
    putToStdErr,
    putLnToStdOut,
    putLnToStdErr,

    -- * Constructors

    -- ** Builder manipulators
    force,
    intercalate,
    intercalateMap,
    padFromLeft,
    padFromRight,

    -- ** Textual
    text,
    lazyText,
    string,
    asciiByteString,
    hexData,

    -- ** Character
    char,

    -- *** Low-level character
    unicodeCodePoint,
    utf16CodeUnits1,
    utf16CodeUnits2,
    utf8CodeUnits1,
    utf8CodeUnits2,
    utf8CodeUnits3,
    utf8CodeUnits4,

    -- ** Integers

    -- *** Decimal
    decimal,
    unsignedDecimal,
    fixedUnsignedDecimal,
    thousandSeparatedDecimal,
    thousandSeparatedUnsignedDecimal,
    dataSizeInBytesInDecimal,

    -- *** Binary
    unsignedBinary,
    unsignedPaddedBinary,
    finiteBitsUnsignedBinary,

    -- *** Hexadecimal
    hexadecimal,
    unsignedHexadecimal,

    -- ** Digits
    decimalDigit,
    hexadecimalDigit,

    -- ** Real
    fixedDouble,
    doublePercent,

    -- ** Time
    utcTimeInIso8601,
    utcTimestampInIso8601,
    intervalInSeconds,
    diffTimeCompact,
    picosecondsCompact,

    -- * Classes
    IsomorphicToTextBuilder (..),
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified DeferredFolds.Unfoldr as Unfoldr
import qualified Test.QuickCheck.Gen as QcGen
import qualified TextBuilderDev.Allocator as Allocator
import TextBuilderDev.Prelude hiding (intercalate, length, null)

#if MIN_VERSION_text(2,0,2)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- * --

-- |
-- Evidence that there exists an unambiguous way to convert
-- a type to and from "TextBuilder".
--
-- Unlike conversion classes from other libs this class is lawful.
-- The law is:
--
-- @'fromTextBuilder' . 'toTextBuilder' = 'id'@
--
-- This class does not provide implicit rendering,
-- such as from integer to its decimal representation.
-- There are multiple ways of representing an integer
-- as text (e.g., hexadecimal, binary).
-- The non-ambiguity is further enforced by the presence of
-- the inverse conversion.
-- In the integer case there is no way to read it
-- from a textual form without a possibility of failing
-- (e.g., when the input string cannot be parsed as an integer).
--
-- If you're looking for such conversion classes,
-- this library is not a place for them,
-- since there can be infinite amount of flavours of
-- conversions. They are context-dependent and as such
-- should be defined as part of the domain.
class IsomorphicToTextBuilder a where
  toTextBuilder :: a -> TextBuilder
  fromTextBuilder :: TextBuilder -> a

instance IsomorphicToTextBuilder TextBuilder where
  toTextBuilder = id
  fromTextBuilder = id

instance IsomorphicToTextBuilder Text where
  toTextBuilder = text
  fromTextBuilder = buildText

instance IsomorphicToTextBuilder String where
  toTextBuilder = fromString
  fromTextBuilder = Text.unpack . buildText

instance IsomorphicToTextBuilder TextLazy.Text where
  toTextBuilder = lazyText
  fromTextBuilder = TextLazy.fromStrict . buildText

instance IsomorphicToTextBuilder TextLazyBuilder.Builder where
  toTextBuilder = text . TextLazy.toStrict . TextLazyBuilder.toLazyText
  fromTextBuilder = TextLazyBuilder.fromText . buildText

#if MIN_VERSION_text(2,0,2)
instance IsomorphicToTextBuilder TextEncoding.StrictBuilder where
  toTextBuilder = toTextBuilder . TextEncoding.strictBuilderToText
  fromTextBuilder = TextEncoding.textToStrictBuilder . fromTextBuilder
#endif

-- * --

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data TextBuilder
  = TextBuilder
      {-# UNPACK #-} !Allocator.Allocator
      {-# UNPACK #-} !Int

instance Semigroup TextBuilder where
  (<>) (TextBuilder allocator1 sizeInChars1) (TextBuilder allocator2 sizeInChars2) =
    TextBuilder
      (allocator1 <> allocator2)
      (sizeInChars1 + sizeInChars2)
  stimes n (TextBuilder allocator size) =
    TextBuilder (stimes n allocator) (size * fromIntegral n)

instance Monoid TextBuilder where
  {-# INLINE mempty #-}
  mempty = TextBuilder mempty 0

instance IsString TextBuilder where
  fromString = string

instance Show TextBuilder where
  show = Text.unpack . buildText

instance Eq TextBuilder where
  (==) = on (==) buildText

instance Arbitrary TextBuilder where
  arbitrary =
    QcGen.oneof
      [ QcGen.scale (flip div 2)
          $ QcGen.oneof
            [ (<>) <$> arbitrary <*> arbitrary,
              sconcat <$> arbitrary,
              stimes <$> arbitrary @Word8 <*> arbitrary,
              pure mempty,
              mconcat <$> arbitrary
            ],
        text <$> arbitrary,
        lazyText <$> arbitrary,
        string <$> arbitrary,
        asciiByteString . ByteString.filter (< 128) <$> arbitrary,
        hexData <$> arbitrary,
        char <$> arbitrary,
        decimal @Integer <$> arbitrary,
        unsignedDecimal @Natural <$> arbitrary,
        thousandSeparatedDecimal @Integer <$> arbitrary <*> arbitrary,
        thousandSeparatedUnsignedDecimal @Natural <$> arbitrary <*> arbitrary,
        dataSizeInBytesInDecimal @Natural <$> arbitrary <*> arbitrary,
        unsignedBinary @Natural <$> arbitrary,
        unsignedPaddedBinary @Word <$> arbitrary,
        finiteBitsUnsignedBinary @Word <$> arbitrary,
        hexadecimal @Integer <$> arbitrary,
        unsignedHexadecimal @Natural <$> arbitrary,
        decimalDigit <$> QcGen.choose @Int (0, 9),
        hexadecimalDigit <$> QcGen.choose @Int (0, 15),
        fixedDouble <$> QcGen.choose (0, 19) <*> arbitrary,
        doublePercent <$> QcGen.choose (0, 19) <*> arbitrary,
        utcTimestampInIso8601 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        intervalInSeconds @Double <$> arbitrary
      ]

instance IsomorphicTo TextBuilder Text where
  to = TextBuilderDev.text

instance IsomorphicTo Text TextBuilder where
  to = TextBuilderDev.buildText

instance IsomorphicTo TextBuilder TextLazy.Text where
  to = TextBuilderDev.lazyText

instance IsomorphicTo TextLazy.Text TextBuilder where
  to = to . to @Text

instance IsomorphicTo TextBuilder TextLazyBuilder.Builder where
  to = to . to @TextLazy.Text

instance IsomorphicTo TextLazyBuilder.Builder TextBuilder where
  to = to . to @Text

#if MIN_VERSION_text(2,0,2)

instance IsomorphicTo TextBuilder TextEncoding.StrictBuilder where
  to = to . TextEncoding.strictBuilderToText

instance IsomorphicTo TextEncoding.StrictBuilder TextBuilder where
  to = TextEncoding.textToStrictBuilder . to

#endif
-- * Accessors

-- | Get the amount of characters.
{-# INLINE length #-}
length :: TextBuilder -> Int
length (TextBuilder _ x) = x

-- | Check whether the builder is empty.
{-# INLINE null #-}
null :: TextBuilder -> Bool
null = (== 0) . length

-- | Execute a builder producing a strict text.
buildText :: TextBuilder -> Text
buildText (TextBuilder allocator _) =
  Allocator.allocate allocator

-- ** Output IO

-- | Put builder, to stdout.
putToStdOut :: TextBuilder -> IO ()
putToStdOut = Text.hPutStr stdout . buildText

-- | Put builder, to stderr.
putToStdErr :: TextBuilder -> IO ()
putToStdErr = Text.hPutStr stderr . buildText

-- | Put builder, followed by a line, to stdout.
putLnToStdOut :: TextBuilder -> IO ()
putLnToStdOut = Text.hPutStrLn stdout . buildText

-- | Put builder, followed by a line, to stderr.
putLnToStdErr :: TextBuilder -> IO ()
putLnToStdErr = Text.hPutStrLn stderr . buildText

-- * Constructors

-- |
-- Run the builder and pack the produced text into a new builder.
--
-- Useful to have around builders that you reuse,
-- because a forced builder is much faster,
-- since it's virtually a single call @memcopy@.
{-# INLINE force #-}
force :: TextBuilder -> TextBuilder
force = text . buildText

-- | Unicode character.
{-# INLINE char #-}
char :: Char -> TextBuilder
char = unicodeCodePoint . ord

-- | Unicode code point.
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> TextBuilder
unicodeCodePoint a =
  TextBuilder (Allocator.unicodeCodePoint a) 1

-- | Single code-unit UTF-16 character.
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> TextBuilder
utf16CodeUnits1 a =
  TextBuilder (Allocator.utf16CodeUnits1 a) 1

-- | Double code-unit UTF-16 character.
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
utf16CodeUnits2 a b =
  TextBuilder (Allocator.utf16CodeUnits2 a b) 1

-- | Single code-unit UTF-8 character.
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> TextBuilder
utf8CodeUnits1 a =
  TextBuilder (Allocator.utf8CodeUnits1 a) 1

-- | Double code-unit UTF-8 character.
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
utf8CodeUnits2 a b =
  TextBuilder (Allocator.utf8CodeUnits2 a b) 1

-- | Triple code-unit UTF-8 character.
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits3 a b c =
  TextBuilder (Allocator.utf8CodeUnits3 a b c) 1

-- | UTF-8 character out of 4 code units.
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits4 a b c d =
  TextBuilder (Allocator.utf8CodeUnits4 a b c d) 1

-- | ASCII byte string.
--
-- It's your responsibility to ensure that the bytes are in proper range,
-- otherwise the produced text will be broken.
{-# INLINEABLE asciiByteString #-}
asciiByteString :: ByteString -> TextBuilder
asciiByteString byteString =
  TextBuilder
    (Allocator.asciiByteString byteString)
    (ByteString.length byteString)

-- | Strict text.
{-# INLINEABLE text #-}
text :: Text -> TextBuilder
text text =
  TextBuilder (Allocator.text text) (Text.length text)

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

-- | Decimal representation of an integral value.
{-# INLINEABLE decimal #-}
decimal :: (Integral a) => a -> TextBuilder
decimal i =
  if i >= 0
    then unsignedDecimal i
    else unicodeCodePoint 45 <> unsignedDecimal (negate i)

-- | Decimal representation of an unsigned integral value.
{-# INLINEABLE unsignedDecimal #-}
unsignedDecimal :: (Integral a) => a -> TextBuilder
unsignedDecimal =
  foldMap decimalDigit . Unfoldr.decimalDigits

fixedUnsignedDecimal :: (Integral a) => Int -> a -> TextBuilder
fixedUnsignedDecimal size val =
  TextBuilder (Allocator.fixedUnsignedDecimal size val) size

-- | Decimal representation of an integral value with thousands separated by the specified character.
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: (Integral a) => Char -> a -> TextBuilder
thousandSeparatedDecimal separatorChar a =
  if a >= 0
    then thousandSeparatedUnsignedDecimal separatorChar a
    else unicodeCodePoint 45 <> thousandSeparatedUnsignedDecimal separatorChar (negate a)

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character.
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
{-# INLINEABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: (Integral a) => Char -> a -> TextBuilder
dataSizeInBytesInDecimal separatorChar amount =
  if amount < 1000
    then unsignedDecimal amount <> "B"
    else
      if amount < 1000000
        then dividedDecimal separatorChar 100 amount <> "kB"
        else
          if amount < 1000000000
            then dividedDecimal separatorChar 100000 amount <> "MB"
            else
              if amount < 1000000000000
                then dividedDecimal separatorChar 100000000 amount <> "GB"
                else
                  if amount < 1000000000000000
                    then dividedDecimal separatorChar 100000000000 amount <> "TB"
                    else
                      if amount < 1000000000000000000
                        then dividedDecimal separatorChar 100000000000000 amount <> "PB"
                        else
                          if amount < 1000000000000000000000
                            then dividedDecimal separatorChar 100000000000000000 amount <> "EB"
                            else
                              if amount < 1000000000000000000000000
                                then dividedDecimal separatorChar 100000000000000000000 amount <> "ZB"
                                else dividedDecimal separatorChar 100000000000000000000000 amount <> "YB"

dividedDecimal :: (Integral a) => Char -> a -> a -> TextBuilder
dividedDecimal separatorChar divisor n =
  let byDivisor = div n divisor
      byExtraTen = div byDivisor 10
      remainder = byDivisor - byExtraTen * 10
   in if remainder == 0 || byExtraTen >= 10
        then thousandSeparatedDecimal separatorChar byExtraTen
        else thousandSeparatedDecimal separatorChar byExtraTen <> "." <> decimalDigit remainder

-- | Unsigned binary number.
{-# INLINE unsignedBinary #-}
unsignedBinary :: (Integral a) => a -> TextBuilder
unsignedBinary =
  foldMap decimalDigit . Unfoldr.binaryDigits

-- | A less general but faster alternative to 'unsignedBinary'.
finiteBitsUnsignedBinary :: (FiniteBits a) => a -> TextBuilder
finiteBitsUnsignedBinary a =
  TextBuilder allocator size
  where
    allocator = Allocator.finiteBitsUnsignedBinary a
    size = Allocator.sizeBound allocator

-- | Unsigned binary number.
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> TextBuilder
unsignedPaddedBinary a =
  padFromLeft (finiteBitSize a) '0' $ foldMap decimalDigit $ Unfoldr.binaryDigits a

-- | Hexadecimal representation of an integral value.
{-# INLINE hexadecimal #-}
hexadecimal :: (Integral a) => a -> TextBuilder
hexadecimal i =
  if i >= 0
    then unsignedHexadecimal i
    else unicodeCodePoint 45 <> unsignedHexadecimal (negate i)

-- | Unsigned hexadecimal representation of an integral value.
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: (Integral a) => a -> TextBuilder
unsignedHexadecimal =
  foldMap hexadecimalDigit . Unfoldr.hexadecimalDigits

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
{-# INLINEABLE padFromLeft #-}
padFromLeft :: Int -> Char -> TextBuilder -> TextBuilder
padFromLeft paddedLength paddingChar builder =
  let builderLength = length builder
   in if paddedLength <= builderLength
        then builder
        else foldMap char (replicate (paddedLength - builderLength) paddingChar) <> builder

-- | Pad a builder from the right side to the specified length with the specified character.
{-# INLINEABLE padFromRight #-}
padFromRight :: Int -> Char -> TextBuilder -> TextBuilder
padFromRight paddedLength paddingChar builder =
  let builderLength = length builder
   in if paddedLength <= builderLength
        then builder
        else builder <> foldMap char (replicate (paddedLength - builderLength) paddingChar)

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
-- > 2021-11-24T12:11:02Z
--
-- Integrations with various time-libraries can be easily derived from that.
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
    $ padFromLeft 2 '0' (decimal days)
    <> ":"
    <> padFromLeft 2 '0' (decimal hours)
    <> ":"
    <> padFromLeft 2 '0' (decimal minutes)
    <> ":"
    <> padFromLeft 2 '0' (decimal seconds)

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
{-# INLINE doublePercent #-}
doublePercent ::
  -- | Amount of decimals after point.
  Int ->
  Double ->
  TextBuilder
doublePercent decimalPlaces x = fixedDouble decimalPlaces (x * 100) <> "%"

-- | Hexadecimal readable representation of binary data.
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
