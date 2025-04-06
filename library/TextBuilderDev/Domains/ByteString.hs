{-# LANGUAGE CPP #-}

module TextBuilderDev.Domains.ByteString where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import qualified Data.Text.Array as TextArray
import TextBuilderCore
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Other
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Prelude hiding (intercalate, length, null)

#if !MIN_VERSION_text(2,0,0)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- | Hexadecimal readable representation of binary data.
--
-- >>> hexByteString "Hello"
-- "4865 6c6c 6f"
{-# INLINE hexByteString #-}
hexByteString :: ByteString -> TextBuilder
hexByteString =
  intercalate " "
    . fmap mconcat
    . Split.chunksOf 2
    . fmap byte
    . ByteString.unpack
  where
    byte =
      padFromLeft 2 '0' . unsignedHexadecimal

-- | UTF-8 bytestring. You can use it for converting ASCII values as well.
--
-- __Warning:__ It's your responsibility to ensure that the bytestring is properly encoded.
--
-- >>> unsafeUtf8ByteString "abc"
-- "abc"
--
-- >>> import Data.Text.Encoding (encodeUtf8)
-- >>> unsafeUtf8ByteString (encodeUtf8 "фывапролдж") == "фывапролдж"
-- True
{-# INLINEABLE unsafeUtf8ByteString #-}
unsafeUtf8ByteString :: ByteString -> TextBuilder
#if MIN_VERSION_text(2,0,0)
unsafeUtf8ByteString byteString =
  TextBuilder
    (ByteString.length byteString)
    ( \array ->
        -- TODO: Optimize to use memcpy or something similar.
        let step byte next index = do
              TextArray.unsafeWrite array index byte
              next (succ index)
         in ByteString.foldr step return byteString
    )
#else
-- Using a quick and dirty solution here since the old stuff is becoming less important with time.
unsafeUtf8ByteString =
  text . TextEncoding.decodeUtf8
#endif
