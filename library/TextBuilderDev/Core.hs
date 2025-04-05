{-# LANGUAGE CPP #-}

module TextBuilderDev.Core where

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
      --
      -- __Warning:__ The function must not write outside of the allocated array or bad things will happen to the running app.
      --
      -- __Warning:__ Keep in mind that the array is operating on 'Word8' values starting from @text-2.0@, but prior to it it operates on 'Word16'. This is due to the \"text\" library switching from UTF-16 to UTF-8 after version 2. To deal with this you have the following options:
      --
      -- 1. Restrict the version of the \"text\" library in your package to @>=2@.
      --
      -- 2. Use helpers provided by this library, such as 'unsafeSeptets' and 'unsafeReverseSeptets', which abstract over the differences in the underlying representation.
      --
      -- 3. Use CPP to conditionally compile your code for different versions of \"text\".
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

-- * Basic Unsafe Primitives

-- |
-- Provides a unified way to deal with the byte array regardless of the version of the @text@ library.
--
-- Keep in mind that prior to @text-2.0@, the array was operating on 'Word16' values due to the library abstracting over @UTF-16@.
-- Starting from @text-2.0@, the array operates on 'Word8' values and the library abstracts over @UTF-8@.
--
-- This function is useful for building ASCII values.
--
-- >>> unsafeSeptets 3 (fmap (+48) [1, 2, 3])
-- "123"
--
-- >>> unsafeSeptets 4 (fmap (+48) [1, 2, 3])
-- "123"
{-# INLINE unsafeSeptets #-}
unsafeSeptets ::
  -- | Maximum size of the byte array to allocate.
  --
  -- Must be greater than or equal to the length of the list.
  --
  -- __Warning:__ If it is smaller, bad things will happen.
  -- We'll be writing outside of the allocated array.
  Int ->
  -- | List of bytes to write.
  --
  -- __Warning:__ It is your responsibility to ensure that the bytes are smaller than 128.
  -- Otherwise the produced text will have a broken encoding.
  --
  -- To ensure of optimization kicking in it is advised to construct the list using 'GHC.List.build'.
  [Word8] ->
  TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeSeptets maxSize bytes =
  TextBuilder
    maxSize
    ( \array ->
        foldr
          ( \byte next offset -> do
              TextArray.unsafeWrite array offset byte
              next (succ offset)
          )
          return
          bytes
    )
#else
unsafeSeptets maxSize bytes =
  TextBuilder
    maxSize
    ( \array ->
        foldr
          ( \byte next offset -> do
              TextArray.unsafeWrite array offset (fromIntegral byte)
              next (succ offset)
          )
          return
          bytes
    )
#endif

-- | Same as 'unsafeSeptets', but writes the bytes in reverse order and requires the size to be precise.
--
-- >>> unsafeReverseSeptets 3 (fmap (+48) [1, 2, 3])
-- "321"
{-# INLINE unsafeReverseSeptets #-}
unsafeReverseSeptets ::
  -- | Precise amount of bytes in the list.
  --
  -- Needs to be precise, because writing happens in reverse order.
  --
  -- __Warning:__ If it is smaller, bad things will happen.
  -- We'll be writing outside of the allocated array.
  Int ->
  -- | List of bytes to write in reverse order.
  --
  -- __Warning:__ It is your responsibility to ensure that the bytes are smaller than 128.
  -- Otherwise the produced text will have a broken encoding.
  --
  -- To ensure of optimization kicking in it is advised to construct the list using 'GHC.List.build'.
  [Word8] ->
  TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeReverseSeptets preciseSize bytes =
  TextBuilder
    preciseSize
    ( \array startOffset ->
        let endOffset = startOffset + preciseSize
         in foldr
              ( \byte next offset -> do
                  TextArray.unsafeWrite array offset byte
                  next (pred offset)
              )
              (\_ -> return endOffset)
              bytes
              (pred endOffset)
    )
#else
unsafeReverseSeptets preciseSize bytes =
  TextBuilder
    preciseSize
    ( \array startOffset ->
        let endOffset = startOffset + preciseSize
         in foldr
              ( \byte next offset -> do
                  TextArray.unsafeWrite array offset (fromIntegral byte)
                  next (pred offset)
              )
              (\_ -> return endOffset)
              bytes
              (pred endOffset)
    )
#endif
