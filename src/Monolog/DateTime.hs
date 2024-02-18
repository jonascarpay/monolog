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
  = -- 12/7
    DatePartial PartialDate
  | -- 2024-12-07
    DateFull FullDate
  | -- 13:00[ +09:00]
    DateTimeToday TimeOfDay (Maybe TimeZone)
  | -- 3/13 13:00[ +09:00]
    DateTimePartial PartialDate TimeOfDay (Maybe TimeZone)
  | -- 2024-03-13 13:00[ +09:00]
    DateTimeFull FullDate TimeOfDay (Maybe TimeZone)
  | -- 3/14 - 3/18
    DateRangePartial PartialDate PartialDate
  | -- 2024-03-14 - 2024-03-18
    DateRangeFull FullDate FullDate
  | -- 13:00 - 14:00[ +09:00]
    DateTimeRangeToday TimeOfDay TimeOfDay (Maybe TimeZone)
  | -- 12/7 12:00 - [12/8 ]01:00[ +9:00]
    DateTimeRangePartial PartialDate TimeOfDay (Maybe PartialDate) TimeOfDay (Maybe TimeZone)
  | -- 2024-12-07 12:00 - [2024-12-08 ]01:00[ +9:00]
    DateTimeRangeFull FullDate TimeOfDay (Maybe FullDate) TimeOfDay (Maybe TimeZone)
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
