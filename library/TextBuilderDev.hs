module TextBuilderDev
  ( TextBuilder,

    -- * Accessors
    toText,
    toString,
    null,

    -- ** Output IO
    putToStdOut,
    putToStdErr,
    putLnToStdOut,
    putLnToStdErr,

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

    -- ** Integers

    -- *** Decimal
    decimal,
    unsignedDecimal,
    fixedUnsignedDecimal,
    thousandSeparatedDecimal,
    thousandSeparatedUnsignedDecimal,
    dataSizeInBytesInDecimal,

    -- *** Binary
    unsignedBinary,
    unsignedPaddedBinary,
    finiteBitsUnsignedBinary,

    -- *** Octal
    octal,
    unsignedOctal,

    -- *** Hexadecimal
    hexadecimal,
    unsignedHexadecimal,

    -- ** Digits
    decimalDigit,
    hexadecimalDigit,

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
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.StrictBuilder ()
import TextBuilderDev.Domains.StrictTextBuilder ()
import TextBuilderDev.Extras
import TextBuilderDev.IsTextBuilder
