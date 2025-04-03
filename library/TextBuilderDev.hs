module TextBuilderDev
  ( TextBuilder,

    -- * Accessors
    toString,
    toText,
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
    IsomorphicToTextBuilder (..),
  )
where

import TextBuilderDev.Base
import TextBuilderDev.Extras
import TextBuilderDev.IsomorphicToTextBuilder
import TextBuilderDev.IsomorphismClass ()
import TextBuilderDev.LawfulConversions ()
