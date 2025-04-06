module TextBuilderDev
  ( TextBuilder,

    -- * Accessors
    toText,
    toString,
    isEmpty,

    -- * Constructors

    -- ** Transformations
    force,
    intercalate,
    intercalateMap,
    padFromLeft,
    padFromRight,

    -- ** Textual
    text,
    lazyText,
    string,
    unsafeUtf8ByteString,

    -- ** Character
    char,
    unicodeCodepoint,

    -- ** Data
    hexByteString,
    finiteBits,
    paddedFiniteBits,

    -- ** Integers

    -- *** Binary
    binary,

    -- *** Octal
    octal,

    -- *** Decimal
    decimal,
    fixedUnsignedDecimal,
    thousandSeparatedDecimal,

    -- *** Hexadecimal
    hexadecimal,

    -- ** Real
    fixedDouble,
    doublePercent,

    -- ** Time
    utcTimeIso8601Timestamp,
    realFracDdHhMmSsInterval,
    diffTimeInterval,
    picosecondsInterval,

    -- ** Other
    approximateDataSize,

    -- * Classes
    Isomorphic (..),
  )
where

import TextBuilderCore
import TextBuilderDev.Domains.Bits
import TextBuilderDev.Domains.ByteString
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Other
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Domains.StrictBuilder ()
import TextBuilderDev.Domains.StrictTextBuilder ()
import TextBuilderDev.Domains.Time
import TextBuilderDev.Isomorphic
