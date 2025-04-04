{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.Domains.StrictTextBuilder where

#if MIN_VERSION_text(2,1,2)

import Data.Text.Internal.StrictBuilder
import qualified TextBuilderDev.Base as Base
import TextBuilderDev.IsTextBuilder
import TextBuilderDev.Prelude

instance IsTextBuilder StrictTextBuilder where
  {-# INLINE from #-}
  from (StrictTextBuilder size write) =
    Base.TextBuilder
      size
      ( \array offset ->
          write array offset $> offset + size
      )
  {-# INLINE to #-}
  to (Base.TextBuilder size write) =
    StrictTextBuilder
      size
      ( \array offset ->
          write array offset $> ()
      )

#endif
