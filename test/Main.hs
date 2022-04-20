module Main where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as A
import qualified Data.Text.Encoding as Text
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import qualified TextBuilderDev as B
import Prelude hiding (choose)

main =
  defaultMain $
    testGroup "All tests" $
      [ testProperty "ASCII ByteString" $
          let gen = listOf $ do
                list <- listOf (choose (0, 127))
                return (ByteString.pack list)
           in forAll gen $ \chunks ->
                mconcat chunks
                  === Text.encodeUtf8 (B.buildText (foldMap B.asciiByteString chunks)),
        testProperty "Intercalation has the same effect as in Text" $
          \separator texts ->
            A.intercalate separator texts
              === B.buildText (B.intercalate (B.text separator) (fmap B.text texts)),
        testProperty "Packing a list of chars is isomorphic to appending a list of builders" $
          \chars ->
            A.pack chars
              === B.buildText (foldMap B.char chars),
        testProperty "Concatting a list of texts is isomorphic to fold-mapping with builders" $
          \texts ->
            mconcat texts
              === B.buildText (foldMap B.text texts),
        testProperty "Concatting a list of texts is isomorphic to concatting a list of builders" $
          \texts ->
            mconcat texts
              === B.buildText (mconcat (map B.text texts)),
        testProperty "Concatting a list of trimmed texts is isomorphic to concatting a list of builders" $
          \texts ->
            let trimmedTexts = fmap (A.drop 3) texts
             in mconcat trimmedTexts
                  === B.buildText (mconcat (map B.text trimmedTexts)),
        testProperty "Decimal" $ \(x :: Integer) ->
          (fromString . show) x === (B.buildText (B.decimal x)),
        testProperty "Hexadecimal vs std show" $ \(x :: Integer) ->
          x >= 0
            ==> (fromString . showHex x) "" === (B.buildText . B.hexadecimal) x,
        testProperty "TextBuilderDev.null is isomorphic to Text.null" $ \(text :: Text) ->
          B.null (B.toTextBuilder text) === A.null text,
        testProperty "(TextBuilderDev.utf8CodeUnitsN <>) is isomorphic to Text.cons" $ \(text :: Text) (c :: Char) ->
          let mkBuilder
                | cp <    0x80 = B.utf8CodeUnits1 (cuAt 0)
                | cp <   0x800 = B.utf8CodeUnits2 (cuAt 0) (cuAt 1)
                | cp < 0x10000 = B.utf8CodeUnits3 (cuAt 0) (cuAt 1) (cuAt 2)
                | otherwise    = B.utf8CodeUnits4 (cuAt 0) (cuAt 1) (cuAt 2) (cuAt 3)
                where codeUnits = Text.encodeUtf8 $ A.singleton c
                      cuAt = (codeUnits `ByteString.index`)
                      cp = Char.ord c
          in
          B.buildText (mkBuilder <> B.text text) === A.cons c text,
        testCase "Separated thousands" $ do
          assertEqual "" "0" (B.buildText (B.thousandSeparatedUnsignedDecimal ',' 0))
          assertEqual "" "123" (B.buildText (B.thousandSeparatedUnsignedDecimal ',' 123))
          assertEqual "" "1,234" (B.buildText (B.thousandSeparatedUnsignedDecimal ',' 1234))
          assertEqual "" "1,234,567" (B.buildText (B.thousandSeparatedUnsignedDecimal ',' 1234567)),
        testCase "Pad from left" $ do
          assertEqual "" "00" (B.buildText (B.padFromLeft 2 '0' ""))
          assertEqual "" "00" (B.buildText (B.padFromLeft 2 '0' "0"))
          assertEqual "" "01" (B.buildText (B.padFromLeft 2 '0' "1"))
          assertEqual "" "12" (B.buildText (B.padFromLeft 2 '0' "12"))
          assertEqual "" "123" (B.buildText (B.padFromLeft 2 '0' "123")),
        testCase "Pad from right" $ do
          assertEqual "" "00" (B.buildText (B.padFromRight 2 '0' ""))
          assertEqual "" "00" (B.buildText (B.padFromRight 2 '0' "0"))
          assertEqual "" "10" (B.buildText (B.padFromRight 2 '0' "1"))
          assertEqual "" "12" (B.buildText (B.padFromRight 2 '0' "12"))
          assertEqual "" "123" (B.buildText (B.padFromRight 2 '0' "123"))
          assertEqual "" "1  " (B.buildText (B.padFromRight 3 ' ' "1")),
        testCase "Hexadecimal" $
          assertEqual "" "1f23" (B.buildText (B.hexadecimal 0x01f23)),
        testCase "Negative Hexadecimal" $
          assertEqual "" "-1f23" (B.buildText (B.hexadecimal (-0x01f23))),
        testGroup "Time interval" $
          [ testCase "59s" $ assertEqual "" "00:00:00:59" $ B.buildText $ B.intervalInSeconds 59,
            testCase "minute" $ assertEqual "" "00:00:01:00" $ B.buildText $ B.intervalInSeconds 60,
            testCase "90s" $ assertEqual "" "00:00:01:30" $ B.buildText $ B.intervalInSeconds 90,
            testCase "hour" $ assertEqual "" "00:01:00:00" $ B.buildText $ B.intervalInSeconds 3600,
            testCase "day" $ assertEqual "" "01:00:00:00" $ B.buildText $ B.intervalInSeconds 86400
          ],
        testCase "dataSizeInBytesInDecimal" $ do
          assertEqual "" "999B" (B.buildText (B.dataSizeInBytesInDecimal ',' 999))
          assertEqual "" "1kB" (B.buildText (B.dataSizeInBytesInDecimal ',' 1000))
          assertEqual "" "1.1kB" (B.buildText (B.dataSizeInBytesInDecimal ',' 1100))
          assertEqual "" "1.1MB" (B.buildText (B.dataSizeInBytesInDecimal ',' 1150000))
          assertEqual "" "9.9MB" (B.buildText (B.dataSizeInBytesInDecimal ',' 9990000))
          assertEqual "" "10MB" (B.buildText (B.dataSizeInBytesInDecimal ',' 10100000))
          assertEqual "" "1,000YB" (B.buildText (B.dataSizeInBytesInDecimal ',' 1000000000000000000000000000)),
        testCase "fixedDouble" $ do
          assertEqual "" "0.0" (B.buildText (B.fixedDouble 1 0.05))
          assertEqual "" "0.1" (B.buildText (B.fixedDouble 1 0.06))
          assertEqual "" "10.0000" (B.buildText (B.fixedDouble 4 10))
          assertEqual "" "0.9000" (B.buildText (B.fixedDouble 4 0.9)),
        testCase "doublePercent" $ do
          assertEqual "" "90.4%" (B.buildText (B.doublePercent 1 0.904))
      ]
