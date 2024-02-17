{-# LANGUAGE StrictData #-}

module Monolog.Time
  ( PartialDateTime (..),
    PartialDate (..),
    Timestamp (..),
    fromPartialDate,
    fromPartialDateTime,
    fromPartialTimestamp,
  )
where

import Data.Bifunctor
import Data.Ord
import Data.Time
import GHC.Generics (Generic)

fromPartialDate :: Day -> PartialDate -> Day
fromPartialDate _ (Exact dt) = dt
fromPartialDate today@(YearMonthDay yRef _ _) (Partial (Just m) d) =
  let thisYears = YearMonthDay yRef m d
      nextYears = addGregorianYearsRollOver 1 thisYears
   in if thisYears >= today then thisYears else nextYears
fromPartialDate today@(YearMonthDay yRef mRef _) (Partial Nothing d) =
  let thisMonths = YearMonthDay yRef mRef d
      nextMonths = addGregorianMonthsRollOver 1 thisMonths
   in if thisMonths >= today then thisMonths else nextMonths

fromPartialDateTime :: ZonedTime -> PartialDateTime -> ZonedTime
fromPartialDateTime zonedRef (PartialDateTime tod mdt mtz) =
  let ZonedTime (LocalTime dateRef _) tz = case mtz of
        Nothing -> zonedRef
        Just tz' -> utcToZonedTime tz' $ zonedTimeToUTC zonedRef
   in ZonedTime (LocalTime (maybe dateRef (fromPartialDate dateRef) mdt) tod) tz

data Timestamp date datetime
  = Date date
  | DateTime datetime
  | DateRange date date
  | DateTimeRange datetime datetime
  deriving stock (Eq, Show, Functor)

instance Bifunctor Timestamp where
  bimap f _ (Date a) = Date (f a)
  bimap f _ (DateRange a b) = DateRange (f a) (f b)
  bimap _ g (DateTime a) = DateTime (g a)
  bimap _ g (DateTimeRange a b) = DateTimeRange (g a) (g b)

fromPartialTimestamp :: ZonedTime -> Timestamp PartialDate PartialDateTime -> Timestamp Day ZonedTime
fromPartialTimestamp ref@(ZonedTime (LocalTime refDate _) _) ts = case ts of
  Date date -> Date $ fromPartialDate refDate date
  DateTime datetime -> DateTime $ fromPartialDateTime ref datetime
  DateRange a b -> either Date (uncurry DateRange) $ fromPartialRange fromPartialDate id refDate a b
  DateTimeRange a b -> either DateTime (uncurry DateTimeRange) $ fromPartialRange fromPartialDateTime zonedTimeToUTC ref a b

data PartialDate
  = Partial (Maybe MonthOfYear) DayOfMonth
  | Exact Day
  deriving (Eq, Show)

{-# INLINE fromPartialRange #-}
fromPartialRange :: (Ord b) => (exact -> partial -> exact) -> (exact -> b) -> exact -> partial -> partial -> Either exact (exact, exact)
fromPartialRange fromPartial compBy ref lo hi =
  let lo' = fromPartial ref lo
      hi' = fromPartial lo' hi
   in case comparing compBy lo' hi' of
        LT -> Right $ (,) lo' hi'
        EQ -> Left lo'
        GT -> Right $ (,) hi' lo'

data PartialDateTime = PartialDateTime
  { tod :: TimeOfDay,
    date :: Maybe PartialDate,
    tzMinutes :: Maybe TimeZone
  }
  deriving stock (Eq, Show, Generic)
