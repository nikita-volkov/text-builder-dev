{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TextBuilderDev.Domains.Unicode where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.IO as Text
import qualified Data.Text.Internal as TextInternal
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import qualified Test.QuickCheck.Gen as QcGen
import TextBuilderDev.Base
import qualified TextBuilderDev.Domains.Unicode.Utf16View as Utf16View
import qualified TextBuilderDev.Domains.Unicode.Utf8View as Utf8View
import TextBuilderDev.Prelude

-- | Unicode character.
{-# INLINE char #-}
char :: Char -> TextBuilder
char = unicodeCodePoint . ord

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
