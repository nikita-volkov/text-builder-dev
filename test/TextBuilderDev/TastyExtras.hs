{-# OPTIONS_GHC -Wno-orphans #-}

module TextBuilderDev.TastyExtras where

import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified TextBuilderDev as B
import Prelude hiding (choose)

-- * --

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary =
    TextLazyBuilder.fromLazyText <$> arbitrary

-- * --

embedsLaws ::
  (B.Embeds a, Eq a, Show a, Arbitrary a) =>
  String ->
  Proxy a ->
  TestTree
embedsLaws subject proxy =
  testGroup subject
    $ [ testProperty "from . to == id" $ \a ->
          (B.from . flip asProxyTypeOf proxy . B.to) a === a
      ]
