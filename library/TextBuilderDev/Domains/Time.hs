module TextBuilderDev.Domains.Time where

import TextBuilderDev.Core
import TextBuilderDev.Domains.Digits
import TextBuilderDev.Domains.Padding
import TextBuilderDev.Prelude

-- | UTC time in ISO8601 format.
--
-- >>> utcTimeInIso8601 (read "2021-11-24 12:11:02 UTC")
-- "2021-11-24T12:11:02Z"
utcTimeInIso8601 :: UTCTime -> TextBuilder
utcTimeInIso8601 UTCTime {..} =
  let (year, month, day) = toGregorian utctDay
      daySeconds = round utctDayTime
      (dayMinutes, second) = divMod daySeconds 60
      (hour, minute) = divMod dayMinutes 60
   in utcTimestampInIso8601 (fromIntegral year) month day hour minute second

-- |
-- General template for formatting date values according to the ISO8601 standard.
-- The format is the following:
--
-- Integrations with various time-libraries can be easily derived from that.
--
-- >>> utcTimestampInIso8601 2021 11 24 12 11 02
-- "2021-11-24T12:11:02Z"
utcTimestampInIso8601 ::
  -- | Year.
  Int ->
  -- | Month.
  Int ->
  -- | Day.
  Int ->
  -- | Hour.
  Int ->
  -- | Minute.
  Int ->
  -- | Second.
  Int ->
  TextBuilder
utcTimestampInIso8601 y mo d h mi s =
  mconcat
    [ fixedUnsignedDecimal 4 y,
      "-",
      fixedUnsignedDecimal 2 mo,
      "-",
      fixedUnsignedDecimal 2 d,
      "T",
      fixedUnsignedDecimal 2 h,
      ":",
      fixedUnsignedDecimal 2 mi,
      ":",
      fixedUnsignedDecimal 2 s,
      "Z"
    ]

-- |
-- Time interval in seconds.
--
-- The format is the following:
--
-- > DD:HH:MM:SS
--
-- Directly applicable to 'DiffTime' and 'NominalDiffTime'.
{-# INLINEABLE intervalInSeconds #-}
intervalInSeconds :: (RealFrac seconds) => seconds -> TextBuilder
intervalInSeconds interval = flip evalState (round interval :: Int) $ do
  seconds <- state (swap . flip divMod 60)
  minutes <- state (swap . flip divMod 60)
  hours <- state (swap . flip divMod 24)
  days <- get
  return
    ( mconcat
        [ padFromLeft 2 '0' (decimal days),
          ":",
          padFromLeft 2 '0' (decimal hours),
          ":",
          padFromLeft 2 '0' (decimal minutes),
          ":",
          padFromLeft 2 '0' (decimal seconds)
        ]
    )

-- | DiffTime in a compact decimal format based on 'picosecondsCompact'.
diffTimeCompact :: DiffTime -> TextBuilder
diffTimeCompact = picosecondsCompact . diffTimeToPicoseconds

-- | Amount of picoseconds represented in a compact decimal format using suffixes.
--
-- E.g., the following is @1_230_000_000@ picoseconds or 1.23 milliseconds or 1230 microseconds:
--
-- > 1230us
picosecondsCompact :: Integer -> TextBuilder
picosecondsCompact x =
  attemptOr 1_000_000_000_000 "s"
    $ attemptOr 1_000_000_000 "ms"
    $ attemptOr 1_000_000 "us"
    $ attemptOr 1_000 "ns"
    $ decimal x
    <> "ps"
  where
    attemptOr factor suffix alternative =
      if x == divided * factor
        then decimal divided <> suffix
        else alternative
      where
        divided = div x factor
