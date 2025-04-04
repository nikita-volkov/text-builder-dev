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

isTextBuilderLaws ::
  (B.IsTextBuilder a, Eq a, Show a, Arbitrary a) =>
  String ->
  Proxy a ->
  TestTree
isTextBuilderLaws subject proxy =
  testGroup subject
    $ [ testProperty "to . from == id" $ \a ->
          (B.to . B.from) a === asProxyTypeOf a proxy,
        testProperty "from . to == id" $ \a ->
          (B.from . flip asProxyTypeOf proxy . B.to) a === a
      ]
