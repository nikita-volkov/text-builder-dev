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
    thousandSeparatedDecimal,
    thousandSeparatedUnsignedDecimal,
    dataSizeInBytesInDecimal,

    -- *** Binary
    unsignedBinary,
    unsignedPaddedBinary,

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
    utcTimestampInIso8601,
    intervalInSeconds,

    -- * Classes
    IsomorphicToTextBuilder (..),
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.IO as Text
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified DeferredFolds.Unfoldr as Unfoldr
import TextBuilderDev.Prelude hiding (intercalate, length, null)
import qualified TextBuilderDev.Utf16View as Utf16View
import qualified TextBuilderDev.Utf8View as Utf8View

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

-- * Action

newtype Action
  = Action (forall s. TextArray.MArray s -> Int -> ST s Int)

-- * --

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data TextBuilder
  = TextBuilder !Action !Int !Int

instance Semigroup TextBuilder where
  (<>) (TextBuilder (Action writeL) estimatedArraySize1 textLength1) (TextBuilder (Action writeR) estimatedArraySize2 textLength2) =
    TextBuilder action estimatedArraySize textLength
    where
      action = Action $ \array offset -> do
        offsetAfter1 <- writeL array offset
        writeR array offsetAfter1
      estimatedArraySize =
        estimatedArraySize1 + estimatedArraySize2
      textLength =
        textLength1 + textLength2

instance Monoid TextBuilder where
  {-# INLINE mempty #-}
  mempty =
    TextBuilder (Action (const return)) 0 0

instance IsString TextBuilder where
  fromString = string

instance Show TextBuilder where
  show = Text.unpack . buildText

instance Eq TextBuilder where
  (==) = on (==) buildText

instance IsomorphicTo TextBuilder TextBuilder where
  to = id

instance IsomorphicTo TextBuilder String where
  to = TextBuilderDev.string

instance IsomorphicTo TextBuilder Text where
  to = TextBuilderDev.text

instance IsomorphicTo TextBuilder TextLazy.Text where
  to = TextBuilderDev.lazyText

instance IsomorphicTo TextBuilder TextLazyBuilder.Builder where
  to = to . to @TextLazy.Text

instance IsomorphicTo String TextBuilder where
  to = to . to @Text

instance IsomorphicTo Text TextBuilder where
  to = TextBuilderDev.buildText

instance IsomorphicTo TextLazy.Text TextBuilder where
  to = to . to @Text

instance IsomorphicTo TextLazyBuilder.Builder TextBuilder where
  to = to . to @Text

-- * Accessors

-- | Get the amount of characters.
{-# INLINE length #-}
length :: TextBuilder -> Int
length (TextBuilder _ _ x) = x

-- | Check whether the builder is empty.
{-# INLINE null #-}
null :: TextBuilder -> Bool
null = (== 0) . length

-- | Execute a builder producing a strict text.
buildText :: TextBuilder -> Text
buildText (TextBuilder (Action action) sizeBound _) =
  runST $ do
    array <- TextArray.new sizeBound
    offsetAfter <- action array 0
    frozenArray <- TextArray.unsafeFreeze array
    return $ TextInternal.text frozenArray 0 offsetAfter

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

#if MIN_VERSION_text(2,0,0)

-- | Unicode code point.
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> TextBuilder
unicodeCodePoint x =
  Utf8View.unicodeCodePoint x utf8CodeUnits1 utf8CodeUnits2 utf8CodeUnits3 utf8CodeUnits4

{-# INLINEABLE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> TextBuilder
utf8CodeUnits1 unit1 = TextBuilder action 1 1
  where
    action = Action $ \array offset ->
      TextArray.unsafeWrite array offset unit1
        $> succ offset

{-# INLINEABLE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
utf8CodeUnits2 unit1 unit2 = TextBuilder action 2 1
  where
    action = Action $ \array offset -> do
      TextArray.unsafeWrite array (offset + 0) unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      return $ offset + 2

{-# INLINEABLE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits3 unit1 unit2 unit3 = TextBuilder action 3 1
  where
    action = Action $ \array offset -> do
      TextArray.unsafeWrite array (offset + 0) unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      TextArray.unsafeWrite array (offset + 2) unit3
      return $ offset + 3

{-# INLINEABLE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits4 unit1 unit2 unit3 unit4 = TextBuilder action 4 1
  where
    action = Action $ \array offset -> do
      TextArray.unsafeWrite array (offset + 0) unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      TextArray.unsafeWrite array (offset + 2) unit3
      TextArray.unsafeWrite array (offset + 3) unit4
      return $ offset + 4

{-# INLINE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> TextBuilder
utf16CodeUnits1 = unicodeCodePoint . fromIntegral

{-# INLINE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
utf16CodeUnits2 unit1 unit2 = unicodeCodePoint cp
  where
    cp = (((fromIntegral unit1 .&. 0x3FF) `shiftL` 10) .|. (fromIntegral unit2 .&. 0x3FF)) + 0x10000

#else

-- | Unicode code point.
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> TextBuilder
unicodeCodePoint x =
  Utf16View.unicodeCodePoint x utf16CodeUnits1 utf16CodeUnits2

-- | Single code-unit UTF-16 character.
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> TextBuilder
utf16CodeUnits1 unit =
  TextBuilder action 1 1
  where
    action =
      Action $ \array offset ->
        TextArray.unsafeWrite array offset unit
          $> succ offset

-- | Double code-unit UTF-16 character.
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
utf16CodeUnits2 unit1 unit2 =
  TextBuilder action 2 1
  where
    action =
      Action $ \array offset -> do
        TextArray.unsafeWrite array offset unit1
        TextArray.unsafeWrite array (succ offset) unit2
        return $ offset + 2

-- | Single code-unit UTF-8 character.
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> TextBuilder
utf8CodeUnits1 unit1 =
  Utf16View.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2

-- | Double code-unit UTF-8 character.
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
utf8CodeUnits2 unit1 unit2 =
  Utf16View.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2

-- | Triple code-unit UTF-8 character.
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits3 unit1 unit2 unit3 =
  Utf16View.utf8CodeUnits3 unit1 unit2 unit3 utf16CodeUnits1 utf16CodeUnits2

-- | UTF-8 character out of 4 code units.
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  Utf16View.utf8CodeUnits4 unit1 unit2 unit3 unit4 utf16CodeUnits1 utf16CodeUnits2

#endif

-- | ASCII byte string.
--
-- It's your responsibility to ensure that the bytes are in proper range,
-- otherwise the produced text will be broken.
{-# INLINEABLE asciiByteString #-}
asciiByteString :: ByteString -> TextBuilder
asciiByteString byteString =
  TextBuilder action length length
  where
    length = ByteString.length byteString
    action =
      Action $ \array ->
        let step byte next index = do
              TextArray.unsafeWrite array index (fromIntegral byte)
              next (succ index)
         in ByteString.foldr step return byteString

-- | Strict text.
{-# INLINEABLE text #-}
text :: Text -> TextBuilder
#if MIN_VERSION_text(2,0,0)
text text@(TextInternal.Text array offset length) =
  TextBuilder action length (Text.length text)
  where
    action =
      Action $ \builderArray builderOffset -> do
        TextArray.copyI length builderArray builderOffset array offset
        return $ builderOffset + length
#else
text text@(TextInternal.Text array offset length) =
  TextBuilder action length (Text.length text)
  where
    action =
      Action $ \builderArray builderOffset -> do
        TextArray.copyI builderArray builderOffset array offset (builderOffset + length)
        return $ builderOffset + length
#endif

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
decimal :: Integral a => a -> TextBuilder
decimal i =
  if i >= 0
    then unsignedDecimal i
    else unicodeCodePoint 45 <> unsignedDecimal (negate i)

-- | Decimal representation of an unsigned integral value.
{-# INLINEABLE unsignedDecimal #-}
unsignedDecimal :: Integral a => a -> TextBuilder
unsignedDecimal =
  foldMap decimalDigit . Unfoldr.decimalDigits

-- | Decimal representation of an integral value with thousands separated by the specified character.
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: Integral a => Char -> a -> TextBuilder
thousandSeparatedDecimal separatorChar a =
  if a >= 0
    then thousandSeparatedUnsignedDecimal separatorChar a
    else unicodeCodePoint 45 <> thousandSeparatedUnsignedDecimal separatorChar (negate a)

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character.
{-# INLINEABLE thousandSeparatedUnsignedDecimal #-}
thousandSeparatedUnsignedDecimal :: Integral a => Char -> a -> TextBuilder
thousandSeparatedUnsignedDecimal separatorChar a =
  fold $ do
    (index, digit) <- Unfoldr.zipWithReverseIndex $ Unfoldr.decimalDigits a
    if mod index 3 == 0 && index /= 0
      then return (decimalDigit digit <> char separatorChar)
      else return (decimalDigit digit)

-- | Data size in decimal notation over amount of bytes.
{-# INLINEABLE dataSizeInBytesInDecimal #-}
dataSizeInBytesInDecimal :: Integral a => Char -> a -> TextBuilder
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

dividedDecimal :: Integral a => Char -> a -> a -> TextBuilder
dividedDecimal separatorChar divisor n =
  let byDivisor = div n divisor
      byExtraTen = div byDivisor 10
      remainder = byDivisor - byExtraTen * 10
   in if remainder == 0 || byExtraTen >= 10
        then thousandSeparatedDecimal separatorChar byExtraTen
        else thousandSeparatedDecimal separatorChar byExtraTen <> "." <> decimalDigit remainder

-- | Unsigned binary number.
{-# INLINE unsignedBinary #-}
unsignedBinary :: Integral a => a -> TextBuilder
unsignedBinary =
  foldMap decimalDigit . Unfoldr.binaryDigits

-- | Unsigned binary number.
{-# INLINE unsignedPaddedBinary #-}
unsignedPaddedBinary :: (Integral a, FiniteBits a) => a -> TextBuilder
unsignedPaddedBinary a =
  padFromLeft (finiteBitSize a) '0' $ foldMap decimalDigit $ Unfoldr.binaryDigits a

-- | Hexadecimal representation of an integral value.
{-# INLINE hexadecimal #-}
hexadecimal :: Integral a => a -> TextBuilder
hexadecimal i =
  if i >= 0
    then unsignedHexadecimal i
    else unicodeCodePoint 45 <> unsignedHexadecimal (negate i)

-- | Unsigned hexadecimal representation of an integral value.
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: Integral a => a -> TextBuilder
unsignedHexadecimal =
  foldMap hexadecimalDigit . Unfoldr.hexadecimalDigits

-- | Decimal digit.
{-# INLINE decimalDigit #-}
decimalDigit :: Integral a => a -> TextBuilder
decimalDigit n =
  unicodeCodePoint (fromIntegral n + 48)

-- | Hexadecimal digit.
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: Integral a => a -> TextBuilder
hexadecimalDigit n =
  if n <= 9
    then unicodeCodePoint (fromIntegral n + 48)
    else unicodeCodePoint (fromIntegral n + 87)

-- | Intercalate builders.
{-# INLINE intercalate #-}
intercalate :: Foldable f => TextBuilder -> f TextBuilder -> TextBuilder
intercalate separator = extract . foldl' step init
  where
    init = Product2 False mempty
    step (Product2 isNotFirst builder) element =
      Product2 True $
        if isNotFirst
          then builder <> separator <> element
          else element
    extract (Product2 _ builder) = builder

-- | Intercalate projecting values to builder.
{-# INLINE intercalateMap #-}
intercalateMap :: Foldable f => TextBuilder -> (a -> TextBuilder) -> f a -> TextBuilder
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
    [ padFromLeft 4 '0' $ decimal y,
      "-",
      padFromLeft 2 '0' $ decimal mo,
      "-",
      padFromLeft 2 '0' $ decimal d,
      "T",
      padFromLeft 2 '0' $ decimal h,
      ":",
      padFromLeft 2 '0' $ decimal mi,
      ":",
      padFromLeft 2 '0' $ decimal s,
      "Z"
    ]

-- |
-- Time interval in seconds.
-- Directly applicable to 'DiffTime' and 'NominalDiffTime'.
{-# INLINEABLE intervalInSeconds #-}
intervalInSeconds :: RealFrac seconds => seconds -> TextBuilder
intervalInSeconds interval = flip evalState (round interval) $ do
  seconds <- state (swap . flip divMod 60)
  minutes <- state (swap . flip divMod 60)
  hours <- state (swap . flip divMod 24)
  days <- get
  return $
    padFromLeft 2 '0' (decimal days)
      <> ":"
      <> padFromLeft 2 '0' (decimal hours)
      <> ":"
      <> padFromLeft 2 '0' (decimal minutes)
      <> ":"
      <> padFromLeft 2 '0' (decimal seconds)

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
