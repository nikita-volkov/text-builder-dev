{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.Domains.StrictBuilder where

#if MIN_VERSION_text(2,1,2)
#elif MIN_VERSION_text(2,0,2)

import Data.Text.Internal.StrictBuilder
import qualified TextBuilderDev.Base as Base
import TextBuilderDev.IsTextBuilder
import TextBuilderDev.Prelude

instance IsTextBuilder StrictBuilder where
  {-# INLINE from #-}
  from (StrictBuilder size write) =
    Base.TextBuilder
      size
      ( \array offset ->
          write array offset $> offset + size
      )
  {-# INLINE to #-}
  to (Base.TextBuilder size write) =
    StrictBuilder
      size
      ( \array offset ->
          write array offset $> ()
      )

#endif
