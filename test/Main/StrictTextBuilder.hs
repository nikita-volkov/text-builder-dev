{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main.StrictTextBuilder (tests) where

#if MIN_VERSION_text(2,1,2)

import Data.Function
import Data.Proxy
import qualified Data.Text.Encoding as TextEncoding
import qualified Main.TastyExtras as Extras
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))
import Prelude

tests :: [TestTree]
tests =
  [ Extras.isTextBuilderLaws "Strict Text Builder" $ Proxy @TextEncoding.StrictTextBuilder
  ]

instance Eq TextEncoding.StrictTextBuilder where
  a == b =
    on (==) TextEncoding.strictBuilderToText a b

instance Show TextEncoding.StrictTextBuilder where
  showsPrec d =
    showsPrec d . TextEncoding.strictBuilderToText

instance Arbitrary TextEncoding.StrictTextBuilder where
  arbitrary =
    TextEncoding.textToStrictBuilder <$> arbitrary

#else

import Test.Tasty

tests :: [TestTree]
tests = []

#endif
