module Monolog.DateTime
  ( PartialDate (..),
    PartialTimestamp (..),
    Timestamp (..),
    fromPartialTimestamp,
  )
where

import Data.Maybe
import Data.Time

data PartialDate = PartialDate MonthOfYear DayOfMonth
  deriving (Eq, Show)

type FullDate = Day

data Timestamp
  = Date Day
  | DateTime ZonedTime
  | DateRange Day Day
  | DateTimeRange ZonedTime ZonedTime
  deriving (Show)

data PartialTimestamp
  = DatePartial PartialDate
  | DateFull FullDate
  | DateTimeToday TimeOfDay (Maybe TimeZone)
  | DateTimePartial PartialDate TimeOfDay (Maybe TimeZone)
  | DateTimeFull FullDate TimeOfDay (Maybe TimeZone)
  | DateRangePartial PartialDate PartialDate
  | DateRangeFull FullDate FullDate
  | DateTimeRangeToday TimeOfDay TimeOfDay (Maybe TimeZone)
  | DateTimeRangePartial PartialDate TimeOfDay (Maybe PartialDate) TimeOfDay (Maybe TimeZone)
  | DateTimeRangeFull FullDate TimeOfDay (Maybe FullDate) TimeOfDay (Maybe TimeZone)
  deriving (Eq, Show)

fromPartialDate :: Day -> PartialDate -> Maybe Day
fromPartialDate refDate@(YearMonthDay refYear _ _) (PartialDate m d) =
  let thisYears = fromGregorianValid refYear m d
   in if maybe False (>= refDate) thisYears then thisYears else fromGregorianValid (succ refYear) m d

timeRangeOn :: Day -> TimeOfDay -> TimeOfDay -> TimeZone -> Timestamp
timeRangeOn dt a b tz =
  case compare a b of
    LT -> DateTimeRange (ZonedTime (LocalTime dt a) tz) (ZonedTime (LocalTime dt b) tz)
    EQ -> DateTime (ZonedTime (LocalTime dt a) tz)
    GT -> DateTimeRange (ZonedTime (LocalTime dt a) tz) (ZonedTime (LocalTime (succ dt) b) tz)

fromPartialTimestamp :: ZonedTime -> PartialTimestamp -> Maybe Timestamp
fromPartialTimestamp lref@(ZonedTime (LocalTime localDate _) localTz) scrut = case scrut of
  DatePartial pdt -> Date <$> fromPartialDate localDate pdt
  DateFull dt -> Just $ Date dt
  DateTimeToday tod mtz -> do
    let ZonedTime (LocalTime refDate _) tz = fromLocalRef mtz
    Just $ DateTime (ZonedTime (LocalTime refDate tod) tz)
  DateTimePartial pdt tod mtz -> do
    let ZonedTime (LocalTime refDate _) tz = fromLocalRef mtz
    dt <- fromPartialDate refDate pdt
    pure $ DateTime $ ZonedTime (LocalTime dt tod) tz
  DateTimeFull dt tod mtz -> pure $ DateTime $ ZonedTime (LocalTime dt tod) (fromMaybe localTz mtz)
  DateRangePartial a b -> do
    a' <- fromPartialDate localDate a
    b' <- fromPartialDate a' b
    pure $ if a' == b' then Date a' else DateRange a' b'
  DateRangeFull a b ->
    case compare a b of
      LT -> Just $ DateRange a b
      EQ -> Just $ Date a
      GT -> Nothing
  DateTimeRangeToday a b mtz -> do
    let ZonedTime (LocalTime refDate _) tz = fromLocalRef mtz
    Just $ timeRangeOn refDate a b tz
  DateTimeRangePartial pdta a Nothing b mtz -> do
    let ZonedTime (LocalTime refDate _) tz = fromLocalRef mtz
    dta <- fromPartialDate refDate pdta
    Just $ timeRangeOn dta a b tz
  DateTimeRangePartial pdta a (Just pdtb) b mtz -> do
    let ZonedTime (LocalTime refDate _) tz = fromLocalRef mtz
    da <- fromPartialDate refDate pdta
    db <- fromPartialDate da pdtb
    let dta = LocalTime da a
        dtb = LocalTime db b
    case compare dta dtb of
      LT -> pure $ DateTimeRange (ZonedTime dta tz) (ZonedTime dtb tz)
      EQ -> pure $ DateTime (ZonedTime dta tz)
      GT -> Nothing
  DateTimeRangeFull da toda Nothing todb mtz -> do
    let tz = fromMaybe localTz mtz
    pure $ timeRangeOn da toda todb tz
  DateTimeRangeFull da toda (Just db) todb mtz -> do
    let tz = fromMaybe localTz mtz
    let dta = LocalTime da toda
        dtb = LocalTime db todb
    case compare dta dtb of
      LT -> pure $ DateTimeRange (ZonedTime dta tz) (ZonedTime dtb tz)
      EQ -> pure $ DateTime (ZonedTime dta tz)
      GT -> Nothing
  where
    fromLocalRef :: Maybe TimeZone -> ZonedTime
    fromLocalRef mtz = case mtz of
      Just tz' | localTz /= tz' -> utcToZonedTime tz' $ zonedTimeToUTC lref
      _ -> lref

-- dates
-- 12/7
-- 2024-12-07
--
-- datetimes
-- 12/7 13:00 [+9:00]
-- 2024-12-07 13:00 [+9:00]
--
-- date ranges
-- 12/7 - 12/13
-- 2024-12-07 - 2024-12-13
--
-- datetime ranges
-- 12:00 - 01:00 [+9:00]
-- 12/7 12:00 - [12/8] 01:00 [+9:00]
-- 2024-12-07 12:00 - [2024-12-08] 01:00 [+9:00]
