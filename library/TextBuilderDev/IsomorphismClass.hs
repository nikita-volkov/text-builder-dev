{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.IsomorphismClass where

import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import IsomorphismClass
import TextBuilderDev.Base
import TextBuilderDev.Extras
import TextBuilderDev.Prelude hiding (intercalate, length, null)

#if MIN_VERSION_text(2,0,2)
import qualified Data.Text.Encoding as TextEncoding
#endif

-- ** Strict Text

instance IsomorphicTo TextBuilder Text where
  to = text

instance IsomorphicTo Text TextBuilder where
  to = toText

-- ** Lazy Text

instance IsomorphicTo TextBuilder TextLazy.Text where
  to = lazyText

instance IsomorphicTo TextLazy.Text TextBuilder where
  to = to . to @Text

-- ** Lazy Text Builder

instance IsomorphicTo TextBuilder TextLazyBuilder.Builder where
  to = to . to @TextLazy.Text

instance IsomorphicTo TextLazyBuilder.Builder TextBuilder where
  to = to . to @Text

-- ** Strict Text Builder

#if MIN_VERSION_text(2,1,2)

instance IsomorphicTo TextBuilder TextEncoding.StrictTextBuilder where
  to = to . TextEncoding.strictBuilderToText

instance IsomorphicTo TextEncoding.StrictTextBuilder TextBuilder where
  to = TextEncoding.textToStrictBuilder . to

#elif MIN_VERSION_text(2,0,2)

instance IsomorphicTo TextBuilder TextEncoding.StrictBuilder where
  to = to . TextEncoding.strictBuilderToText

instance IsomorphicTo TextEncoding.StrictBuilder TextBuilder where
  to = TextEncoding.textToStrictBuilder . to

#endif
