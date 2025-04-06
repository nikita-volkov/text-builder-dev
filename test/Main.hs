module Main where

import qualified Data.ByteString as ByteString
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word
import qualified Features
import Numeric.Compat
import Numeric.Natural
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import qualified TextBuilderDev as B
import Util.ExtraInstances ()
import Util.TestTrees
import Prelude

main :: IO ()
main = (defaultMain . testGroup "All") tests

tests :: [TestTree]
tests =
  [ testGroup "Features" Features.tests,
    testProperty "ASCII ByteString" $
      let gen = listOf $ do
            list <- listOf (choose (0, 127))
            return (ByteString.pack list)
       in forAll gen $ \chunks ->
            mconcat chunks
              === Text.encodeUtf8 (B.toText (foldMap B.unsafeUtf8ByteString chunks)),
    testProperty "Intercalation has the same effect as in Text" $
      \separator texts ->
        Text.intercalate separator texts
          === B.toText (B.intercalate (B.text separator) (fmap B.text texts)),
    testProperty "intercalateMap sep mapper == intercalate sep . fmap mapper" $
      \separator ints ->
        Text.intercalate separator (fmap (fromString . show @Int) ints)
          === B.toText (B.intercalateMap (B.text separator) B.decimal ints),
    testGroup "By function name" $
      [ testCase "padFromLeft" $ do
          assertEqual "" "00" (B.toText (B.padFromLeft 2 '0' ""))
          assertEqual "" "00" (B.toText (B.padFromLeft 2 '0' "0"))
          assertEqual "" "01" (B.toText (B.padFromLeft 2 '0' "1"))
          assertEqual "" "12" (B.toText (B.padFromLeft 2 '0' "12"))
          assertEqual "" "123" (B.toText (B.padFromLeft 2 '0' "123")),
        testCase "padFromRight" $ do
          assertEqual "" "00" (B.toText (B.padFromRight 2 '0' ""))
          assertEqual "" "00" (B.toText (B.padFromRight 2 '0' "0"))
          assertEqual "" "10" (B.toText (B.padFromRight 2 '0' "1"))
          assertEqual "" "12" (B.toText (B.padFromRight 2 '0' "12"))
          assertEqual "" "123" (B.toText (B.padFromRight 2 '0' "123"))
          assertEqual "" "1  " (B.toText (B.padFromRight 3 ' ' "1")),
        testProperty "decimal" $ \(x :: Integer) ->
          (fromString . show) x === (B.toText (B.decimal x)),
        testGroup "hexadecimal" $
          [ testProperty "show isomorphism" $ \(x :: Integer) ->
              x >= 0 ==>
                (fromString . showHex x) "" === (B.toText . B.hexadecimal @Integer) x,
            testCase "Positive" $
              assertEqual "" "1f23" (B.toText (B.hexadecimal @Integer 0x01f23)),
            testCase "Negative" $
              assertEqual "" "-1f23" (B.toText (B.hexadecimal @Integer (-0x01f23)))
          ],
        testCase "approximateDataSize" $ do
          assertEqual "" "999B" (B.toText (B.approximateDataSize @Integer 999))
          assertEqual "" "1kB" (B.toText (B.approximateDataSize @Integer 1000))
          assertEqual "" "1.1kB" (B.toText (B.approximateDataSize @Integer 1100))
          assertEqual "" "1.1MB" (B.toText (B.approximateDataSize @Integer 1150000))
          assertEqual "" "9.9MB" (B.toText (B.approximateDataSize @Integer 9990000))
          assertEqual "" "10MB" (B.toText (B.approximateDataSize @Integer 10100000))
          assertEqual "" "1,000YB" (B.toText (B.approximateDataSize @Integer 1000000000000000000000000000)),
        testCase "fixedDouble" $ do
          assertEqual "" "0.0" (B.toText (B.fixedDouble 1 0.05))
          assertEqual "" "0.1" (B.toText (B.fixedDouble 1 0.06))
          assertEqual "" "10.0000" (B.toText (B.fixedDouble 4 10))
          assertEqual "" "0.9000" (B.toText (B.fixedDouble 4 0.9)),
        testCase "doublePercent" $ do
          assertEqual "" "90.4%" (B.toText (B.doublePercent 1 0.904)),
        testGroup "finiteBits" $
          [ testProperty "Produces the same output as showBin" $ \(x :: Word) ->
              fromString (showBin x "")
                === B.toText (B.finiteBits x)
          ],
        testGroup "fixedUnsignedDecimal" $
          [ testProperty "Same as printf" $ \(size :: Word8, val :: Natural) ->
              let rendered = show val
                  renderedLength = length rendered
                  intSize = fromIntegral size
                  padded =
                    if renderedLength > intSize
                      then drop (renderedLength - intSize) rendered
                      else replicate (intSize - renderedLength) '0' <> rendered
               in fromString padded
                    === B.toText (B.fixedUnsignedDecimal (fromIntegral size) val)
          ],
        testGroup "utcTimeIso8601Timestamp" $
          [ testProperty "Same as iso8601Show" $ \x ->
              let roundedToSecondsTime =
                    x {utctDayTime = (fromIntegral @Int . round . utctDayTime) x}
               in (fromString . flip mappend "Z" . take 19 . iso8601Show) roundedToSecondsTime
                    === B.toText (B.utcTimeIso8601Timestamp roundedToSecondsTime)
          ]
      ],
    testGroup "Isomorphic instances" $
      [ testGroup "Text" $
          [ isomorphic $ Proxy @Text
          ],
        testGroup "Lazy Text" $
          [ isomorphic $ Proxy @TextLazy.Text
          ],
        testGroup "Lazy Text Builder" $
          [ isomorphic $ Proxy @TextLazyBuilder.Builder
          ]
      ],
    testLaws $ showLaws (Proxy @B.TextBuilder),
    testLaws $ eqLaws (Proxy @B.TextBuilder),
    testLaws $ semigroupLaws (Proxy @B.TextBuilder),
    testLaws $ monoidLaws (Proxy @B.TextBuilder)
  ]

testLaws :: Laws -> TestTree
testLaws Laws {..} =
  testProperties lawsTypeclass lawsProperties
