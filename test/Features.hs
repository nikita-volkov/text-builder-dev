module Features (tests) where

import qualified Data.ByteString as ByteString
import Data.Int
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word
import qualified Features.StrictBuilder as StrictBuilder
import qualified Features.StrictTextBuilder as StrictTextBuilder
import Numeric
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck
import TextBuilderDev
import Util.TestTrees
import Prelude

tests :: [TestTree]
tests =
  [ testGroup "StrictBuilder" StrictBuilder.tests,
    testGroup "StrictTextBuilder" StrictTextBuilder.tests,
    testGroup "binary" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int binary
          ]
      ],
    testGroup "decimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int decimal
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer decimal,
            testProperty "Encodes the same as show" $ \(x :: Integer) ->
              (fromString . show) x === (toText (decimal x))
          ]
      ],
    testGroup "fixedDecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word (fixedDecimal 42)
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural (fixedDecimal 42),
            testProperty "Encodes the same as printf" $ \(size :: Word8, val :: Natural) ->
              let rendered = show val
                  renderedLength = length rendered
                  intSize = fromIntegral size
                  padded =
                    if renderedLength > intSize
                      then drop (renderedLength - intSize) rendered
                      else replicate (intSize - renderedLength) '0' <> rendered
               in fromString padded
                    === toText (fixedDecimal (fromIntegral size) val)
          ]
      ],
    testGroup "thousandSeparatedDecimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int (thousandSeparatedDecimal ',')
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer (thousandSeparatedDecimal ',')
          ]
      ],
    testGroup "hexadecimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int hexadecimal,
            testProperty "Encodes the same as showHex" $ \(x :: Int32) ->
              x >= 0 ==>
                (padFromLeft 8 '0' . string . showHex x) "" === hexadecimal x
          ]
      ],
    testGroup "fixedDouble" $
      [ mapsToMonoid (fixedDouble 3)
      ],
    testGroup "doublePercent" $
      [ mapsToMonoid (doublePercent 3)
      ],
    testGroup "utcTimeIso8601Timestamp" $
      [ mapsToMonoid utcTimeIso8601Timestamp,
        testProperty "Same as iso8601Show" $ \x ->
          let roundedToSecondsTime =
                x {utctDayTime = (fromIntegral @Int . round . utctDayTime) x}
           in (fromString . flip mappend "Z" . take 19 . iso8601Show) roundedToSecondsTime
                === toText (utcTimeIso8601Timestamp roundedToSecondsTime)
      ],
    testGroup "approximateDataSize" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word approximateDataSize
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural approximateDataSize
          ]
      ],
    testGroup "unsafeUtf8ByteString" $
      [ mapsToMonoid unsafeUtf8ByteString,
        testProperty "Works on ASCII" $
          let gen = listOf do
                list <- listOf (choose (0, 127))
                return (ByteString.pack list)
           in forAll gen \chunks ->
                mconcat chunks
                  === Text.encodeUtf8 (toText (foldMap unsafeUtf8ByteString chunks))
      ],
    testGroup "intercalate" $
      [ customGenMonoid do
          sep <- arbitrary
          texts <- listOf arbitrary
          return (intercalate (text sep) (fmap text texts)),
        testProperty "Has the same effect as in Text" $
          \separator texts ->
            Text.intercalate separator texts
              === toText (intercalate (text separator) (fmap text texts))
      ],
    testGroup "intercalateMap" $
      [ customGenMonoid do
          sep <- arbitrary
          texts <- listOf arbitrary
          return (intercalateMap (text sep) text texts),
        testProperty "intercalateMap sep mapper == intercalate sep . fmap mapper" $
          \separator ints ->
            Text.intercalate separator (fmap (fromString . show @Int) ints)
              === toText (intercalateMap (text separator) decimal ints)
      ]
  ]
