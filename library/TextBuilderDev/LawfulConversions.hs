{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.LawfulConversions where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import LawfulConversions
import TextBuilderDev.Base
import TextBuilderDev.Extras
import TextBuilderDev.Prelude hiding (intercalate, length, null)

#if MIN_VERSION_text(2,0,2)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- ** Strict Text

instance IsSome TextBuilder Text where
  to = text

instance IsSome Text TextBuilder where
  to = toText

instance IsMany TextBuilder Text

instance IsMany Text TextBuilder

instance Is TextBuilder Text

instance Is Text TextBuilder

-- ** String

instance IsSome String TextBuilder where
  to = Text.unpack . toText
  maybeFrom = fmap text . maybeFrom

instance IsMany String TextBuilder where
  from = text . from

-- ** Lazy Text

instance IsSome TextBuilder TextLazy.Text where
  to = lazyText

instance IsSome TextLazy.Text TextBuilder where
  to = TextLazy.fromStrict . toText

instance IsMany TextBuilder TextLazy.Text

instance IsMany TextLazy.Text TextBuilder

instance Is TextBuilder TextLazy.Text

instance Is TextLazy.Text TextBuilder

-- ** Lazy Text Builder

instance IsSome TextBuilder TextLazyBuilder.Builder where
  to = text . TextLazy.toStrict . TextLazyBuilder.toLazyText

instance IsSome TextLazyBuilder.Builder TextBuilder where
  to = TextLazyBuilder.fromText . toText

instance IsMany TextBuilder TextLazyBuilder.Builder

instance IsMany TextLazyBuilder.Builder TextBuilder

instance Is TextBuilder TextLazyBuilder.Builder

instance Is TextLazyBuilder.Builder TextBuilder

-- ** Strict Text Builder

#if MIN_VERSION_text(2,1,2)

instance IsSome TextBuilder TextEncoding.StrictTextBuilder where
  to = text . TextEncoding.strictBuilderToText

instance IsSome TextEncoding.StrictTextBuilder TextBuilder where
  to = TextEncoding.textToStrictBuilder . toText

instance IsMany TextBuilder TextEncoding.StrictTextBuilder

instance IsMany TextEncoding.StrictTextBuilder TextBuilder

instance Is TextBuilder TextEncoding.StrictTextBuilder

instance Is TextEncoding.StrictTextBuilder TextBuilder

#elif MIN_VERSION_text(2,0,2)

instance IsSome TextBuilder TextEncoding.StrictBuilder where
  to = text . TextEncoding.strictBuilderToText

instance IsSome TextEncoding.StrictBuilder TextBuilder where
  to = TextEncoding.textToStrictBuilder . toText

instance IsMany TextBuilder TextEncoding.StrictBuilder

instance IsMany TextEncoding.StrictBuilder TextBuilder

instance Is TextBuilder TextEncoding.StrictBuilder

instance Is TextEncoding.StrictBuilder TextBuilder

#endif
