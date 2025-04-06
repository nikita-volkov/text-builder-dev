module Features (tests) where

import qualified Features.StrictBuilder as StrictBuilder
import qualified Features.StrictTextBuilder as StrictTextBuilder
import Numeric.Natural (Natural)
import Test.QuickCheck.Instances ()
import Test.Tasty
import TextBuilderDev
import Util.TestTrees
import Prelude

tests :: [TestTree]
tests =
  [ testGroup "StrictBuilder" StrictBuilder.tests,
    testGroup "StrictTextBuilder" StrictTextBuilder.tests,
    testGroup "finiteBits" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int finiteBits
          ]
      ],
    testGroup "paddedFiniteBits" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int paddedFiniteBits
          ]
      ],
    testGroup "binary" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int binary
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer binary
          ]
      ],
    testGroup "decimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int decimal
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer decimal
          ]
      ],
    testGroup "fixedUnsignedDecimal" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word (fixedUnsignedDecimal 42)
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural (fixedUnsignedDecimal 42)
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
    testGroup "approximateDataSize" $
      [ testGroup "Word" $
          [ mapsToMonoid @Word approximateDataSize
          ],
        testGroup "Natural" $
          [ mapsToMonoid @Natural approximateDataSize
          ]
      ],
    testGroup "hexadecimal" $
      [ testGroup "Int" $
          [ mapsToMonoid @Int hexadecimal
          ],
        testGroup "Integer" $
          [ mapsToMonoid @Integer hexadecimal
          ]
      ],
    testGroup "fixedDouble" $
      [ mapsToMonoid (fixedDouble 3)
      ],
    testGroup "doublePercent" $
      [ mapsToMonoid (doublePercent 3)
      ],
    testGroup "utcTimeIso8601Timestamp" $
      [ mapsToMonoid utcTimeIso8601Timestamp
      ]
  ]
