{-# LANGUAGE CPP #-}

module TextBuilderDev.Embeds where

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
-- @'from' . 'to' = 'id'@
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
class Embeds a where
  -- | Project the type into "TextBuilder". Surjective.
  from :: a -> TextBuilder

  -- | Embed TextBuilder into the type. Injective.
  to :: TextBuilder -> a

instance Embeds TextBuilder where
  from = id
  to = id

instance Embeds Text where
  from = text
  to = toText

instance Embeds String where
  from = fromString
  to = Text.unpack . toText

instance Embeds TextLazy.Text where
  from = lazyText
  to = TextLazy.fromStrict . toText

instance Embeds TextLazyBuilder.Builder where
  from = text . TextLazy.toStrict . TextLazyBuilder.toLazyText
  to = TextLazyBuilder.fromText . toText

#if MIN_VERSION_text(2,1,2)

instance Embeds TextEncoding.StrictTextBuilder where
  from = from . TextEncoding.strictBuilderToText
  to = TextEncoding.textToStrictBuilder . to

#elif MIN_VERSION_text(2,0,2)

instance Embeds TextEncoding.StrictBuilder where
  from = from . TextEncoding.strictBuilderToText
  to = TextEncoding.textToStrictBuilder . to

#endif
