module TextBuilderDev.IsTextBuilder where

import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import TextBuilderDev.Core
import TextBuilderDev.Domains.Other
import TextBuilderDev.Prelude

-- |
-- Evidence that there exists an unambiguous way to convert
-- a type to and from "TextBuilder".
--
-- The laws are:
--
-- - @'from' . 'to' = 'id'@
--
-- - @'to' . 'from' = 'id'@
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
class IsTextBuilder a where
  -- | Project the type into "TextBuilder".
  from :: a -> TextBuilder

  -- | Embed "TextBuilder" into the type.
  to :: TextBuilder -> a

instance IsTextBuilder TextBuilder where
  from = id
  to = id

instance IsTextBuilder Text where
  from = text
  to = toText

instance IsTextBuilder TextLazy.Text where
  from = lazyText
  to = TextLazy.fromStrict . toText

instance IsTextBuilder TextLazyBuilder.Builder where
  from = lazyText . TextLazyBuilder.toLazyText
  to = TextLazyBuilder.fromText . toText
