{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TextBuilderDev.Base
  ( -- * Execution
    toText,
    toMaxSize,
    null,

    -- * Definition
    TextBuilder,
    text,
    asciiByteString,
    unicodeCodePoint,
    utf8CodeUnits1,
    utf8CodeUnits2,
    utf8CodeUnits3,
    utf8CodeUnits4,
    utf16CodeUnits1,
    utf16CodeUnits2,
    finiteBitsUnsignedBinary,
    fixedUnsignedDecimal,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.IO as Text
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Test.QuickCheck.Gen as QcGen
import qualified TextBuilderDev.Base.Utf16View as Utf16View
import qualified TextBuilderDev.Base.Utf8View as Utf8View
import TextBuilderDev.Prelude hiding (null)

-- | Execute the builder producing a strict text.
toText :: TextBuilder -> Text
toText (TextBuilder maxSize write) =
  runST $ do
    array <- TextArray.new maxSize
    offsetAfter <- write array 0
    frozenArray <- TextArray.unsafeFreeze array
    return $ TextInternal.text frozenArray 0 offsetAfter

-- | Estimate the maximum amount of bytes that the produced text can take.
toMaxSize :: TextBuilder -> Int
toMaxSize (TextBuilder maxSize _) = maxSize

-- | Check whether the builder is empty.
{-# INLINE null #-}
null :: TextBuilder -> Bool
null = (== 0) . toMaxSize

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data TextBuilder
  = TextBuilder
      !Int
      !(forall s. TextArray.MArray s -> Int -> ST s Int)

instance IsString TextBuilder where
  fromString = text . fromString

instance Show TextBuilder where
  show = show . toText

instance Eq TextBuilder where
  (==) = on (==) toText

instance Semigroup TextBuilder where
  {-# INLINE (<>) #-}
  (<>) (TextBuilder estimatedArraySizeL writeL) (TextBuilder estimatedArraySizeR writeR) =
    TextBuilder
      (estimatedArraySizeL + estimatedArraySizeR)
      ( \array offset -> do
          offsetAfter1 <- writeL array offset
          writeR array offsetAfter1
      )
  stimes n (TextBuilder maxSize write) =
    TextBuilder
      (maxSize * fromIntegral n)
      ( \array ->
          let go n offset =
                if n > 0
                  then do
                    offset <- write array offset
                    go (pred n) offset
                  else return offset
           in go n
      )

instance Monoid TextBuilder where
  {-# INLINE mempty #-}
  mempty = TextBuilder 0 (const return)
  {-# INLINE mconcat #-}
  mconcat list =
    TextBuilder
      (foldl' (\acc (TextBuilder maxSize _) -> acc + maxSize) 0 list)
      ( \array ->
          let go [] offset = return offset
              go (TextBuilder _ write : xs) offset = do
                offsetAfter <- write array offset
                go xs offsetAfter
           in go list
      )

instance Arbitrary TextBuilder where
  arbitrary = text <$> arbitrary

-- | Strict text.
{-# INLINEABLE text #-}
text :: Text -> TextBuilder
#if MIN_VERSION_text(2,0,0)
text (TextInternal.Text array offset length) =
  TextBuilder length \builderArray builderOffset -> do
    TextArray.copyI length builderArray builderOffset array offset
    return $ builderOffset + length
#else
text (TextInternal.Text array offset length) =
  TextBuilder length \builderArray builderOffset -> do
    let builderOffsetAfter = builderOffset + length
    TextArray.copyI builderArray builderOffset array offset builderOffsetAfter
    return builderOffsetAfter
#endif

-- | ASCII byte string.
--
-- It's your responsibility to ensure that the bytes are in proper range,
-- otherwise the produced text will be broken.
{-# INLINEABLE asciiByteString #-}
asciiByteString :: ByteString -> TextBuilder
asciiByteString byteString =
  TextBuilder
    (ByteString.length byteString)
    ( \array ->
        let step byte next index = do
              TextArray.unsafeWrite array index (fromIntegral byte)
              next (succ index)
         in ByteString.foldr step return byteString
    )

-- | Unicode code point.
--
-- __Warning:__ It is your responsibility to ensure that the code point is in proper range,
-- otherwise the produced text will be broken.
-- It must be in the range of 0x0000 to 0x10FFFF.
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> TextBuilder
#if MIN_VERSION_text(2,0,0)
unicodeCodePoint x =
  Utf8View.unicodeCodePoint x utf8CodeUnits1 utf8CodeUnits2 utf8CodeUnits3 utf8CodeUnits4
#else
unicodeCodePoint x =
  Utf16View.unicodeCodePoint x utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Single code-unit UTF-8 character.
utf8CodeUnits1 :: Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits1 #-}
utf8CodeUnits1 unit1 =
  TextBuilder 1 \array offset ->
    TextArray.unsafeWrite array offset unit1
      $> succ offset
#else
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 unit1 =
  Utf16View.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Double code-unit UTF-8 character.
utf8CodeUnits2 :: Word8 -> Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits2 #-}
utf8CodeUnits2 unit1 unit2 =
  TextBuilder 2 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (offset + 1) unit2
    return $ offset + 2
#else
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 unit1 unit2 =
  Utf16View.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Triple code-unit UTF-8 character.
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits3 #-}
utf8CodeUnits3 unit1 unit2 unit3 =
  TextBuilder 3 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (offset + 1) unit2
    TextArray.unsafeWrite array (offset + 2) unit3
    return $ offset + 3
#else
{-# INLINE utf8CodeUnits3 #-}
utf8CodeUnits3 unit1 unit2 unit3 =
  Utf16View.utf8CodeUnits3 unit1 unit2 unit3 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | UTF-8 character out of 4 code units.
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits4 #-}
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  TextBuilder 4 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (offset + 1) unit2
    TextArray.unsafeWrite array (offset + 2) unit3
    TextArray.unsafeWrite array (offset + 3) unit4
    return $ offset + 4
#else
{-# INLINE utf8CodeUnits4 #-}
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  Utf16View.utf8CodeUnits4 unit1 unit2 unit3 unit4 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Single code-unit UTF-16 character.
utf16CodeUnits1 :: Word16 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINE utf16CodeUnits1 #-}
utf16CodeUnits1 = unicodeCodePoint . fromIntegral
#else
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 unit =
  TextBuilder 1 \array offset ->
    TextArray.unsafeWrite array offset unit
      $> succ offset
#endif

-- | Double code-unit UTF-16 character.
utf16CodeUnits2 :: Word16 -> Word16 -> TextBuilder
#if MIN_VERSION_text(2,0,0)
{-# INLINE utf16CodeUnits2 #-}
utf16CodeUnits2 unit1 unit2 = unicodeCodePoint cp
  where
    cp = (((fromIntegral unit1 .&. 0x3FF) `shiftL` 10) .|. (fromIntegral unit2 .&. 0x3FF)) + 0x10000
#else
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 unit1 unit2 =
  TextBuilder 2 \array offset -> do
    TextArray.unsafeWrite array offset unit1
    TextArray.unsafeWrite array (succ offset) unit2
    return $ offset + 2
#endif

-- | A less general but faster alternative to 'unsignedBinary'.
finiteBitsUnsignedBinary :: (FiniteBits a) => a -> TextBuilder
finiteBitsUnsignedBinary val =
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

-- | Fixed-length decimal.
-- Padded with zeros or trimmed depending on whether it's shorter or longer
-- than specified.
--
-- __Warning:__ It is your responsibility to ensure that the size is positive and in a reasonable range,
-- and that the value is positive, otherwise the produced text will be broken.
--
-- >>> fixedUnsignedDecimal 5 123
-- "00123"
--
-- >>> fixedUnsignedDecimal 5 123456
-- "23456"
--
-- >>> fixedUnsignedDecimal 0 123
-- ""
fixedUnsignedDecimal :: (Integral a) => Int -> a -> TextBuilder
fixedUnsignedDecimal size val =
  TextBuilder size $ \array startOffset ->
    let offsetAfter = startOffset + size
        writeValue val offset =
          if offset >= startOffset
            then
              if val /= 0
                then case divMod val 10 of
                  (val, digit) -> do
                    TextArray.unsafeWrite array offset $ 48 + fromIntegral digit
                    writeValue val (pred offset)
                else writePadding offset
            else return offsetAfter
        writePadding offset =
          if offset >= startOffset
            then do
              TextArray.unsafeWrite array offset 48
              writePadding (pred offset)
            else return offsetAfter
     in writeValue val (pred offsetAfter)
