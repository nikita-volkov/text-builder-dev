module Main where

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Builder as TextLazyBuilder
import Numeric.Compat
import Test.QuickCheck.Classes
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))
import qualified TextBuilderDev as B
import qualified TextBuilderDev.TastyExtras as Extras
import Prelude hiding (choose, showBin)

main :: IO ()
main =
  defaultMain
    $ testGroup "All tests"
    $ [ testProperty "ASCII ByteString"
          $ let gen = listOf $ do
                  list <- listOf (choose (0, 127))
                  return (ByteString.pack list)
             in forAll gen $ \chunks ->
                  mconcat chunks
                    === Text.encodeUtf8 (B.buildText (foldMap B.asciiByteString chunks)),
        testProperty "Intercalation has the same effect as in Text"
          $ \separator texts ->
            Text.intercalate separator texts
              === B.buildText (B.intercalate (B.text separator) (fmap B.text texts)),
        testProperty "intercalateMap sep mapper == intercalate sep . fmap mapper"
          $ \separator ints ->
            Text.intercalate separator (fmap (fromString . show @Int) ints)
              === B.buildText (B.intercalateMap (B.text separator) B.decimal ints),
        testProperty "Packing a list of chars is isomorphic to appending a list of builders"
          $ \chars ->
            Text.pack chars
              === B.buildText (foldMap B.char chars),
        testProperty "Concatting a list of texts is isomorphic to fold-mapping with builders"
          $ \texts ->
            mconcat texts
              === B.buildText (foldMap B.text texts),
        testProperty "Concatting a list of texts is isomorphic to concatting a list of builders"
          $ \texts ->
            mconcat texts
              === B.buildText (mconcat (map B.text texts)),
        testProperty "Concatting a list of trimmed texts is isomorphic to concatting a list of builders"
          $ \texts ->
            let trimmedTexts = fmap (Text.drop 3) texts
             in mconcat trimmedTexts
                  === B.buildText (mconcat (map B.text trimmedTexts)),
        testProperty "TextBuilderDev.null is isomorphic to Text.null" $ \(text :: Text) ->
          B.null (B.toTextBuilder text) === Text.null text,
        testProperty "(TextBuilderDev.unicodeCodePoint <>) is isomorphic to Text.cons"
          $ withMaxSuccess bigTest
          $ \(text :: Text) (c :: Char) ->
            B.buildText (B.unicodeCodePoint (Char.ord c) <> B.text text) === Text.cons c text,
        testGroup "Time interval"
          $ [ testCase "59s" $ assertEqual "" "00:00:00:59" $ B.buildText $ B.intervalInSeconds @Double 59,
              testCase "minute" $ assertEqual "" "00:00:01:00" $ B.buildText $ B.intervalInSeconds @Double 60,
              testCase "90s" $ assertEqual "" "00:00:01:30" $ B.buildText $ B.intervalInSeconds @Double 90,
              testCase "hour" $ assertEqual "" "00:01:00:00" $ B.buildText $ B.intervalInSeconds @Double 3600,
              testCase "day" $ assertEqual "" "01:00:00:00" $ B.buildText $ B.intervalInSeconds @Double 86400
            ],
        testGroup "By function name"
          $ [ testGroup "utf8CodeUnitsN"
                $ [ testProperty "Text.cons isomporphism"
                      $ withMaxSuccess bigTest
                      $ \(text :: Text) (c :: Char) ->
                        let cp = Char.ord c
                            cuBuilder
                              | cp < 0x80 = B.utf8CodeUnits1 (cuAt 0)
                              | cp < 0x800 = B.utf8CodeUnits2 (cuAt 0) (cuAt 1)
                              | cp < 0x10000 = B.utf8CodeUnits3 (cuAt 0) (cuAt 1) (cuAt 2)
                              | otherwise = B.utf8CodeUnits4 (cuAt 0) (cuAt 1) (cuAt 2) (cuAt 3)
                              where
                                -- Use Data.Text.Encoding for comparison
                                codeUnits = Text.encodeUtf8 $ Text.singleton c
                                cuAt = (codeUnits `ByteString.index`)
                         in B.buildText (cuBuilder <> B.text text) === Text.cons c text,
                    testProperty "Text.singleton isomorphism"
                      $ withMaxSuccess bigTest
                      $ \(c :: Char) ->
                        let text = Text.singleton c
                            codeUnits = Text.encodeUtf8 text
                            cp = Char.ord c
                            cuBuilder
                              | cp < 0x80 = B.utf8CodeUnits1 (cuAt 0)
                              | cp < 0x800 = B.utf8CodeUnits2 (cuAt 0) (cuAt 1)
                              | cp < 0x10000 = B.utf8CodeUnits3 (cuAt 0) (cuAt 1) (cuAt 2)
                              | otherwise = B.utf8CodeUnits4 (cuAt 0) (cuAt 1) (cuAt 2) (cuAt 3)
                              where
                                cuAt = ByteString.index codeUnits
                         in B.buildText cuBuilder === text
                  ],
              testGroup "utf16CodeUnitsN"
                $ [ testProperty "is isomorphic to Text.cons"
                      $ withMaxSuccess bigTest
                      $ \(text :: Text) (c :: Char) ->
                        let cp = Char.ord c
                            cuBuilder
                              | cp < 0x10000 = B.utf16CodeUnits1 (cuAt 0)
                              | otherwise = B.utf16CodeUnits2 (cuAt 0) (cuAt 1)
                              where
                                -- Use Data.Text.Encoding for comparison
                                codeUnits = Text.encodeUtf16LE $ Text.singleton c
                                cuAt i =
                                  (fromIntegral $ codeUnits `ByteString.index` (2 * i))
                                    .|. ((fromIntegral $ codeUnits `ByteString.index` (2 * i + 1)) `shiftL` 8)
                         in B.buildText (cuBuilder <> B.text text) === Text.cons c text
                  ],
              testCase "thousandSeparatedUnsignedDecimal" $ do
                assertEqual "" "0" (B.buildText (B.thousandSeparatedUnsignedDecimal @Integer ',' 0))
                assertEqual "" "123" (B.buildText (B.thousandSeparatedUnsignedDecimal @Integer ',' 123))
                assertEqual "" "1,234" (B.buildText (B.thousandSeparatedUnsignedDecimal @Integer ',' 1234))
                assertEqual "" "1,234,567" (B.buildText (B.thousandSeparatedUnsignedDecimal @Integer ',' 1234567)),
              testCase "padFromLeft" $ do
                assertEqual "" "00" (B.buildText (B.padFromLeft 2 '0' ""))
                assertEqual "" "00" (B.buildText (B.padFromLeft 2 '0' "0"))
                assertEqual "" "01" (B.buildText (B.padFromLeft 2 '0' "1"))
                assertEqual "" "12" (B.buildText (B.padFromLeft 2 '0' "12"))
                assertEqual "" "123" (B.buildText (B.padFromLeft 2 '0' "123")),
              testCase "padFromRight" $ do
                assertEqual "" "00" (B.buildText (B.padFromRight 2 '0' ""))
                assertEqual "" "00" (B.buildText (B.padFromRight 2 '0' "0"))
                assertEqual "" "10" (B.buildText (B.padFromRight 2 '0' "1"))
                assertEqual "" "12" (B.buildText (B.padFromRight 2 '0' "12"))
                assertEqual "" "123" (B.buildText (B.padFromRight 2 '0' "123"))
                assertEqual "" "1  " (B.buildText (B.padFromRight 3 ' ' "1")),
              testProperty "decimal" $ \(x :: Integer) ->
                (fromString . show) x === (B.buildText (B.decimal x)),
              testGroup "hexadecimal"
                $ [ testProperty "show isomorphism" $ \(x :: Integer) ->
                      x >= 0 ==>
                        (fromString . showHex x) "" === (B.buildText . B.hexadecimal @Integer) x,
                    testCase "Positive"
                      $ assertEqual "" "1f23" (B.buildText (B.hexadecimal @Integer 0x01f23)),
                    testCase "Negative"
                      $ assertEqual "" "-1f23" (B.buildText (B.hexadecimal @Integer (-0x01f23)))
                  ],
              testCase "dataSizeInBytesInDecimal" $ do
                assertEqual "" "999B" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 999))
                assertEqual "" "1kB" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 1000))
                assertEqual "" "1.1kB" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 1100))
                assertEqual "" "1.1MB" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 1150000))
                assertEqual "" "9.9MB" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 9990000))
                assertEqual "" "10MB" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 10100000))
                assertEqual "" "1,000YB" (B.buildText (B.dataSizeInBytesInDecimal @Integer ',' 1000000000000000000000000000)),
              testCase "fixedDouble" $ do
                assertEqual "" "0.0" (B.buildText (B.fixedDouble 1 0.05))
                assertEqual "" "0.1" (B.buildText (B.fixedDouble 1 0.06))
                assertEqual "" "10.0000" (B.buildText (B.fixedDouble 4 10))
                assertEqual "" "0.9000" (B.buildText (B.fixedDouble 4 0.9)),
              testCase "doublePercent" $ do
                assertEqual "" "90.4%" (B.buildText (B.doublePercent 1 0.904)),
              testGroup "unsignedBinary"
                $ [ testProperty "Produces the same output as showBin" $ \(x :: Natural) ->
                      fromString (showBin x "")
                        === B.buildText (B.unsignedBinary x)
                  ],
              testGroup "finiteBitsUnsignedBinary"
                $ [ testProperty "Produces the same output as showBin" $ \(x :: Word) ->
                      fromString (showBin x "")
                        === B.buildText (B.finiteBitsUnsignedBinary x)
                  ],
              testGroup "fixedUnsignedDecimal"
                $ [ testProperty "Same as printf" $ \(size :: Word8, val :: Natural) ->
                      let rendered = show val
                          renderedLength = length rendered
                          intSize = fromIntegral size
                          padded =
                            if renderedLength > intSize
                              then drop (renderedLength - intSize) rendered
                              else replicate (intSize - renderedLength) '0' <> rendered
                       in fromString padded
                            === B.buildText (B.fixedUnsignedDecimal (fromIntegral size) val)
                  ],
              testGroup "utcTimeInIso8601"
                $ [ testProperty "Same as iso8601Show" $ \x ->
                      let roundedToSecondsTime =
                            x {utctDayTime = (fromIntegral @Int . round . utctDayTime) x}
                       in (fromString . flip mappend "Z" . take 19 . iso8601Show) roundedToSecondsTime
                            === B.buildText (B.utcTimeInIso8601 roundedToSecondsTime)
                  ]
            ],
        testGroup "IsomorphicToTextBuilder instances"
          $ [ Extras.isomorphismLaws "Text" $ Proxy @Text,
              Extras.isomorphismLaws "Lazy Text" $ Proxy @TextLazy.Text,
              Extras.isomorphismLaws "Lazy Text Builder" $ Proxy @TextLazyBuilder.Builder,
              Extras.isomorphismLaws "String" $ Proxy @String
            ],
        testLaws $ showLaws (Proxy @B.TextBuilder),
        testLaws $ eqLaws (Proxy @B.TextBuilder),
        testLaws $ semigroupLaws (Proxy @B.TextBuilder),
        testLaws $ monoidLaws (Proxy @B.TextBuilder)
      ]
  where
    bigTest = 10000

testLaws :: Laws -> TestTree
testLaws Laws {..} =
  testProperties lawsTypeclass lawsProperties
