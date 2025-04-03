module TextBuilderDev.Allocator.ArrayWriter where

import qualified Data.Text.Array as TextArray
import TextBuilderDev.Prelude

-- | Action that writes to a mutable array.
newtype ArrayWriter
  = ArrayWriter (forall s. TextArray.MArray s -> Int -> ST s Int)

instance Semigroup ArrayWriter where
  {-# INLINE (<>) #-}
  ArrayWriter writeL <> ArrayWriter writeR =
    ArrayWriter $ \array offset -> do
      offsetAfter1 <- writeL array offset
      writeR array offsetAfter1
  stimes n (ArrayWriter write) =
    ArrayWriter $ \array ->
      let go n offset =
            if n > 0
              then do
                offset <- write array offset
                go (pred n) offset
              else return offset
       in go n

instance Monoid ArrayWriter where
  {-# INLINE mempty #-}
  mempty = ArrayWriter $ const $ return
