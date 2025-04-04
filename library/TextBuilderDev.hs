module TextBuilderDev
  ( TextBuilder (..),

    -- * Accessors
    toText,
    toString,
    null,

    -- * Constructors

    -- ** Builder manipulators
    force,
    intercalate,
    intercalateMap,
    padFromLeft,
    padFromRight,

    -- ** Textual
    text,
    lazyText,
    string,
    asciiByteString,

    -- ** Data
    bits,
    hexData,

    -- ** Character
    char,

    -- *** Low-level character
    unicodeCodePoint,
    utf16CodeUnits1,
    utf16CodeUnits2,
    utf8CodeUnits1,
    utf8CodeUnits2,
    utf8CodeUnits3,
    utf8CodeUnits4,

    -- ** Digits
    decimalDigit,
    hexadecimalDigit,

    -- ** Integers

    -- *** Binary
    binary,
    unsignedBinary,

    -- *** Octal
    octal,
    unsignedOctal,

    -- *** Decimal
    decimal,
    unsignedDecimal,
    fixedUnsignedDecimal,
    thousandSeparatedDecimal,
    thousandSeparatedUnsignedDecimal,
    dataSizeInBytesInDecimal,

    -- *** Hexadecimal
    hexadecimal,
    unsignedHexadecimal,

    -- ** Real
    fixedDouble,
    doublePercent,

    -- ** Time
    utcTimeInIso8601,
    utcTimestampInIso8601,
    intervalInSeconds,
    diffTimeCompact,
    picosecondsCompact,

    -- * Classes
    IsTextBuilder (..),
  )
where

import TextBuilderDev.Base
import TextBuilderDev.Domains.Data
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Other
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Domains.StrictBuilder ()
import TextBuilderDev.Domains.StrictTextBuilder ()
import TextBuilderDev.Domains.Time
import TextBuilderDev.Domains.Unicode
import TextBuilderDev.IsTextBuilder
