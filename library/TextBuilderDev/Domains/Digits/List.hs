module TextBuilderDev.Domains.Digits.List where

import TextBuilderDev.Prelude

{-# INLINE reverseDigits #-}
reverseDigits ::
  (Integral a) =>
  -- | Radix
  a ->
  -- | Number
  a ->
  [a]
reverseDigits radix = go
  where
    go x = case divMod x radix of
      (next, digit) -> digit : if next <= 0 then [] else go next

-- |
-- Convert a number to a list of digits in the given radix allocating the list.
--
-- >>> digits 10 12345
-- [1,2,3,4,5]
--
-- >>> digits 10 0
-- [0]
--
-- >>> digits 2 1
-- [1]
--
-- >>> digits 2 2
-- [1,0]
--
-- >>> digits 2 0
-- [0]
digits ::
  (Integral a) =>
  -- | Radix
  a ->
  -- | Number
  a ->
  [a]
digits radix = go []
  where
    go !acc x = case divMod x radix of
      (next, digit) ->
        if next <= 0
          then digit : acc
          else go (digit : acc) next
