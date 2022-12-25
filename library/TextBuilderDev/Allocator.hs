module TextBuilderDev.Allocator
  ( -- * Execution
    allocate,

    -- * Definition
    Allocator,
    force,
    text,
    char,
    unicodeCodePoint,
    utf8CodeUnits1,
    utf8CodeUnits2,
    utf8CodeUnits3,
    utf8CodeUnits4,
    utf16CodeUnits1,
    utf16CodeUnits2,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.IO as Text
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import TextBuilderDev.Prelude
import qualified TextBuilderDev.Utf16View as Utf16View
import qualified TextBuilderDev.Utf8View as Utf8View

-- * Action

newtype Action
  = Action (forall s. TextArray.MArray s -> Int -> ST s Int)

instance Semigroup Action where
  {-# INLINE (<>) #-}
  Action writeL <> Action writeR =
    Action $ \array offset -> do
      offsetAfter1 <- writeL array offset
      writeR array offsetAfter1

instance Monoid Action where
  {-# INLINE mempty #-}
  mempty = Action $ const $ return

-- * Allocator

-- | Execute a builder producing a strict text.
allocate :: Allocator -> Text
allocate (Allocator (Action action) sizeBound) =
  runST $ do
    array <- TextArray.new sizeBound
    offsetAfter <- action array 0
    frozenArray <- TextArray.unsafeFreeze array
    return $ TextInternal.text frozenArray 0 offsetAfter

-- |
-- Specification of how to efficiently construct strict 'Text'.
-- Provides instances of 'Semigroup' and 'Monoid', which have complexity of /O(1)/.
data Allocator
  = Allocator !Action !Int

instance Semigroup Allocator where
  (<>) (Allocator action1 estimatedArraySize1) (Allocator action2 estimatedArraySize2) =
    Allocator action estimatedArraySize
    where
      action = action1 <> action2
      estimatedArraySize = estimatedArraySize1 + estimatedArraySize2

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

-- | Strict text.
{-# INLINEABLE text #-}
text :: Text -> Allocator
#if MIN_VERSION_text(2,0,0)
text text@(TextInternal.Text array offset length) =
  Allocator action length
  where
    action =
      Action $ \builderArray builderOffset -> do
        TextArray.copyI length builderArray builderOffset array offset
        return $ builderOffset + length
#else
text text@(TextInternal.Text array offset length) =
  Allocator action length
  where
    action =
      Action $ \builderArray builderOffset -> do
        let builderOffsetAfter = builderOffset + length
        TextArray.copyI builderArray builderOffset array offset builderOffsetAfter
        return builderOffsetAfter
#endif

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
{-# INLINEABLE utf8CodeUnits1 #-}
utf8CodeUnits1 :: Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
utf8CodeUnits1 unit1 = Allocator action 1 
  where
    action = Action $ \array offset ->
      TextArray.unsafeWrite array offset unit1
        $> succ offset
#else
utf8CodeUnits1 unit1 =
  Utf16View.utf8CodeUnits1 unit1 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Double code-unit UTF-8 character.
{-# INLINEABLE utf8CodeUnits2 #-}
utf8CodeUnits2 :: Word8 -> Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
utf8CodeUnits2 unit1 unit2 = Allocator action 2 
  where
    action = Action $ \array offset -> do
      TextArray.unsafeWrite array offset unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      return $ offset + 2
#else
utf8CodeUnits2 unit1 unit2 =
  Utf16View.utf8CodeUnits2 unit1 unit2 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Triple code-unit UTF-8 character.
{-# INLINEABLE utf8CodeUnits3 #-}
utf8CodeUnits3 :: Word8 -> Word8 -> Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
utf8CodeUnits3 unit1 unit2 unit3 = Allocator action 3 
  where
    action = Action $ \array offset -> do
      TextArray.unsafeWrite array offset unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      TextArray.unsafeWrite array (offset + 2) unit3
      return $ offset + 3
#else
utf8CodeUnits3 unit1 unit2 unit3 =
  Utf16View.utf8CodeUnits3 unit1 unit2 unit3 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | UTF-8 character out of 4 code units.
{-# INLINEABLE utf8CodeUnits4 #-}
utf8CodeUnits4 :: Word8 -> Word8 -> Word8 -> Word8 -> Allocator
#if MIN_VERSION_text(2,0,0)
utf8CodeUnits4 unit1 unit2 unit3 unit4 = Allocator action 4 
  where
    action = Action $ \array offset -> do
      TextArray.unsafeWrite array offset unit1
      TextArray.unsafeWrite array (offset + 1) unit2
      TextArray.unsafeWrite array (offset + 2) unit3
      TextArray.unsafeWrite array (offset + 3) unit4
      return $ offset + 4
#else
utf8CodeUnits4 unit1 unit2 unit3 unit4 =
  Utf16View.utf8CodeUnits4 unit1 unit2 unit3 unit4 utf16CodeUnits1 utf16CodeUnits2
#endif

-- | Single code-unit UTF-16 character.
{-# INLINEABLE utf16CodeUnits1 #-}
utf16CodeUnits1 :: Word16 -> Allocator
#if MIN_VERSION_text(2,0,0)
utf16CodeUnits1 = unicodeCodePoint . fromIntegral
#else
utf16CodeUnits1 unit =
  Allocator action 1
  where
    action =
      Action $ \array offset ->
        TextArray.unsafeWrite array offset unit
          $> succ offset
#endif

-- | Double code-unit UTF-16 character.
{-# INLINEABLE utf16CodeUnits2 #-}
utf16CodeUnits2 :: Word16 -> Word16 -> Allocator
#if MIN_VERSION_text(2,0,0)
utf16CodeUnits2 unit1 unit2 = unicodeCodePoint cp
  where
    cp = (((fromIntegral unit1 .&. 0x3FF) `shiftL` 10) .|. (fromIntegral unit2 .&. 0x3FF)) + 0x10000
#else
utf16CodeUnits2 unit1 unit2 =
  Allocator action 2
  where
    action =
      Action $ \array offset -> do
        TextArray.unsafeWrite array offset unit1
        TextArray.unsafeWrite array (succ offset) unit2
        return $ offset + 2
#endif
