module TextBuilderDev.TastyExtras where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified TextBuilderDev as B
import Prelude hiding (choose)

-- * --

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary =
    TextLazyBuilder.fromLazyText <$> arbitrary

instance Arbitrary B.TextBuilder where
  arbitrary =
    B.lazyText <$> arbitrary

-- * --

isomorphismLaws ::
  (B.IsomorphicToTextBuilder a, Eq a, Show a, Arbitrary a) =>
  String ->
  Proxy a ->
  TestTree
isomorphismLaws subject proxy =
  testGroup subject $
    [ testProperty "fromTextBuilder . toTextBuilder == id" $
        (===)
          <$> B.fromTextBuilder . B.toTextBuilder
          <*> flip asProxyTypeOf proxy,
      testProperty "toTextBuilder . fromTextBuilder == id" $
        (===)
          <$> B.toTextBuilder . flip asProxyTypeOf proxy . B.fromTextBuilder
          <*> id
    ]
