{-# LANGUAGE CPP #-}

module TextBuilderDev.IsomorphicToTextBuilder where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import TextBuilderDev.Base
import TextBuilderDev.Extras
import TextBuilderDev.Prelude hiding (intercalate, length, null)

#if MIN_VERSION_text(2,0,2)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- |
-- Evidence that there exists an unambiguous way to convert
-- a type to and from "TextBuilder".
--
-- Unlike conversion classes from other libs this class is lawful.
-- The law is:
--
-- @'toTextBuilder' . 'fromTextBuilder' = 'id'@
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
  fromTextBuilder = toText

instance IsomorphicToTextBuilder String where
  toTextBuilder = fromString
  fromTextBuilder = Text.unpack . toText

instance IsomorphicToTextBuilder TextLazy.Text where
  toTextBuilder = lazyText
  fromTextBuilder = TextLazy.fromStrict . toText

instance IsomorphicToTextBuilder TextLazyBuilder.Builder where
  toTextBuilder = text . TextLazy.toStrict . TextLazyBuilder.toLazyText
  fromTextBuilder = TextLazyBuilder.fromText . toText

#if MIN_VERSION_text(2,1,2)

instance IsomorphicToTextBuilder TextEncoding.StrictTextBuilder where
  toTextBuilder = toTextBuilder . TextEncoding.strictBuilderToText
  fromTextBuilder = TextEncoding.textToStrictBuilder . fromTextBuilder

#elif MIN_VERSION_text(2,0,2)

instance IsomorphicToTextBuilder TextEncoding.StrictBuilder where
  toTextBuilder = toTextBuilder . TextEncoding.strictBuilderToText
  fromTextBuilder = TextEncoding.textToStrictBuilder . fromTextBuilder

#endif
