{-# LANGUAGE CPP #-}

module TextBuilderDev.Allocator
  ( -- * Execution
    allocate,
    sizeBound,

    -- * Definition
    Allocator,
    force,
    text,
    asciiByteString,
    char,
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
import TextBuilderDev.Prelude
import qualified TextBuilderDev.Utf16View as Utf16View
import qualified TextBuilderDev.Utf8View as Utf8View

-- * ArrayWriter

newtype ArrayWriter
  = ArrayWriter (forall s. TextArray.MArray s -> Int -> ST s Int)

instance Semigroup ArrayWriter where
  {-# INLINE (<>) #-}
  ArrayWriter writeL <> ArrayWriter writeR =
    ArrayWriter $ \array offset -> do
      offsetAfter1 <- writeL array offset
      writeR array offsetAfter1
  stimes n (ArrayWriter write) =
    ArrayWriter $ \array ->
      let go n offset =
            if n > 0
              then do
                offset <- write array offset
                go (pred n) offset
              else return offset
       in go n

instance Monoid ArrayWriter where
  {-# INLINE mempty #-}
  mempty = ArrayWriter $ const $ return

-- * Allocator

-- | Execute a builder producing a strict text.
allocate :: Allocator -> Text
allocate (Allocator (ArrayWriter write) sizeBound) =
  runST $ do
    array <- TextArray.new sizeBound
    offsetAfter <- write array 0
    frozenArray <- TextArray.unsafeFreeze array
    return $ TextInternal.text frozenArray 0 offsetAfter

sizeBound :: Allocator -> Int
sizeBound (Allocator _ sizeBound) = sizeBound

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data Allocator
  = Allocator
      !ArrayWriter
      {-# UNPACK #-} !Int

instance Semigroup Allocator where
  {-# INLINE (<>) #-}
  (<>) (Allocator writer1 estimatedArraySize1) (Allocator writer2 estimatedArraySize2) =
    Allocator writer estimatedArraySize
    where
      writer = writer1 <> writer2
      estimatedArraySize = estimatedArraySize1 + estimatedArraySize2
  stimes n (Allocator writer sizeBound) =
    Allocator
      (stimes n writer)
      (sizeBound * fromIntegral n)

instance Monoid Allocator where
  {-# INLINE mempty #-}
  mempty = Allocator mempty 0

-- |
-- Run the builder and pack the produced text into a new builder.
--
-- Useful to have around builders that you reuse,
-- because a forced builder is much faster,
-- since it's virtually a single call @memcopy@.
{-# INLINE force #-}
force :: Allocator -> Allocator
force = text . allocate

{-# INLINE sizedWriter #-}
sizedWriter :: Int -> (forall s. TextArray.MArray s -> Int -> ST s Int) -> Allocator
sizedWriter size write =
  Allocator (ArrayWriter write) size

-- | Strict text.
{-# INLINEABLE text #-}
text :: Text -> Allocator
#if MIN_VERSION_text(2,0,0)
text text@(TextInternal.Text array offset length) =
  Allocator writer length
  where
    writer =
      ArrayWriter $ \builderArray builderOffset -> do
        TextArray.copyI length builderArray builderOffset array offset
        return $ builderOffset + length
#else
text text@(TextInternal.Text array offset length) =
  Allocator writer length
  where
    writer =
      ArrayWriter $ \builderArray builderOffset -> do
        let builderOffsetAfter = builderOffset + length
        TextArray.copyI builderArray builderOffset array offset builderOffsetAfter
        return builderOffsetAfter
#endif

-- | ASCII byte string.
--
-- It's your responsibility to ensure that the bytes are in proper range,
-- otherwise the produced text will be broken.
{-# INLINEABLE asciiByteString #-}
asciiByteString :: ByteString -> Allocator
asciiByteString byteString =
  Allocator action length
  where
    length = ByteString.length byteString
    action =
      ArrayWriter $ \array ->
        let step byte next index = do
              TextArray.unsafeWrite array index (fromIntegral byte)
              next (succ index)
         in ByteString.foldr step return byteString

-- | Unicode character.
{-# INLINE char #-}
char :: Char -> Allocator
char = unicodeCodePoint . ord

-- | Unicode code point.
{-# INLINE unicodeCodePoint #-}
unicodeCodePoint :: Int -> Allocator
#if MIN_VERSION_text(2,0,0)
unicodeCodePoint x =
  Utf8View.unicodeCodePoint x utf8CodeUnits1 utf8CodeUnits2 utf8CodeUnits3 utf8CodeUnits4
#else
unicodeCodePoint x =
  Utf16View.unicodeCodePoint x utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Single code-unit UTF-8 character.
utf8CodeUnits1 :: Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits1 #-}
utf8CodeUnits1 unit1 = Allocator writer 1 
  where
    writer = ArrayWriter $ \array offset ->
      TextArray.unsafeWrite array offset unit1
        $> succ offset
#else
{-# INLINE utf8CodeUnits1 #-}
utf8CodeUnits1 unit1 =
  Utf16View.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Double code-unit UTF-8 character.
utf8CodeUnits2 :: Word8 -> Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits2 #-}
utf8CodeUnits2 unit1 unit2 = Allocator writer 2 
  where
    writer = ArrayWriter $ \array offset -> do
      TextArray.unsafeWrite array offset unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      return $ offset + 2
#else
{-# INLINE utf8CodeUnits2 #-}
utf8CodeUnits2 unit1 unit2 =
  Utf16View.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Triple code-unit UTF-8 character.
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits3 #-}
utf8CodeUnits3 unit1 unit2 unit3 = Allocator writer 3 
  where
    writer = ArrayWriter $ \array offset -> do
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
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
{-# INLINEABLE utf8CodeUnits4 #-}
utf8CodeUnits4 unit1 unit2 unit3 unit4 = Allocator writer 4 
  where
    writer = ArrayWriter $ \array offset -> do
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
utf16CodeUnits1 :: Word16 -> Allocator
#if MIN_VERSION_text(2,0,0)
{-# INLINE utf16CodeUnits1 #-}
utf16CodeUnits1 = unicodeCodePoint . fromIntegral
#else
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 unit =
  Allocator writer 1
  where
    writer =
      ArrayWriter $ \array offset ->
        TextArray.unsafeWrite array offset unit
          $> succ offset
#endif

-- | Double code-unit UTF-16 character.
utf16CodeUnits2 :: Word16 -> Word16 -> Allocator
#if MIN_VERSION_text(2,0,0)
{-# INLINE utf16CodeUnits2 #-}
utf16CodeUnits2 unit1 unit2 = unicodeCodePoint cp
  where
    cp = (((fromIntegral unit1 .&. 0x3FF) `shiftL` 10) .|. (fromIntegral unit2 .&. 0x3FF)) + 0x10000
#else
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 unit1 unit2 =
  Allocator writer 2
  where
    writer =
      ArrayWriter $ \array offset -> do
        TextArray.unsafeWrite array offset unit1
        TextArray.unsafeWrite array (succ offset) unit2
        return $ offset + 2
#endif

-- | A less general but faster alternative to 'unsignedBinary'.
finiteBitsUnsignedBinary :: FiniteBits a => a -> Allocator
finiteBitsUnsignedBinary val =
  Allocator writer size
  where
    writer =
      ArrayWriter $ \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex $
                    if testBit val 0 then 49 else 48
                  go (unsafeShiftR val 1) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)
    size =
      max 1 (finiteBitSize val - countLeadingZeros val)

-- | Fixed-length decimal.
-- Padded with zeros or trimmed depending on whether it's shorter or longer
-- than specified.
fixedUnsignedDecimal :: Integral a => Int -> a -> Allocator
fixedUnsignedDecimal size val =
  sizedWriter size $ \array startOffset ->
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
