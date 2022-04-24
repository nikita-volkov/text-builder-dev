module TextBuilderDev.TastyExtras where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified TextBuilderDev as B
import Prelude hiding (choose)

-- *

instance Arbitrary TextLazyBuilder.Builder where
  arbitrary =
    TextLazyBuilder.fromLazyText <$> arbitrary

-- *

isomorphismLaws :: (B.IsomorphicToTextBuilder a, Eq a, Show a) => String -> Gen a -> TestTree
isomorphismLaws subject gen =
  testGroup subject $
    [ testProperty "fromTextBuilder . toTextBuilder == id" $
        forAll gen $
          (===) <$> B.fromTextBuilder . B.toTextBuilder <*> id
    ]
