{-# LANGUAGE CPP #-}

module TextBuilderDev.Base where

import qualified Data.ByteString as ByteString
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal as TextInternal
import TextBuilderDev.Prelude hiding (null)

-- |
-- Specification of how to efficiently construct strict 'Text'.
--
-- For this task it is much more efficient than @Data.Text.Lazy.Builder.'Data.Text.Lazy.Builder.Builder'@ and even the recently introduced @Data.Text.Encoding.'Data.Text.Encoding.StrictTextBuilder'@.
--
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data TextBuilder
  = TextBuilder
      -- | Estimated maximum size of the byte array to allocate.
      Int
      -- | Function that populates a preallocated byte array of the estimated maximum size specified above provided an offset into it and producing the offset after.
      (forall s. TextArray.MArray s -> Int -> ST s Int)

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

-- * Destructors

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

-- * Constructors

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
