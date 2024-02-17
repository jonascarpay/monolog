{-# LANGUAGE StrictData #-}

module Monolog.Time
  ( PartialDateTime (..),
    PartialDate (..),
    fromPartialDate,
    fromPartialDateTime,
    Timestamp (..),
    fromPartialTimestamp,
    parseTimeStamps,
  )
where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Text.Internal.Fusion
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

-- TODO decide where to validate
data PartialDateTime = PartialDateTime
  { tod :: TimeOfDay,
    date :: Maybe PartialDate,
    tzMinutes :: Maybe TimeZone
  }
  deriving stock (Eq, Show, Generic)

newtype Parser a = Parser (forall s r. (s -> Step s Char) -> s -> (a -> s -> r) -> r -> r)
  deriving (Functor)

instance Applicative Parser where
  pure a = Parser $ \_ s ok _ -> ok a s
  {-# INLINE (<*>) #-}
  (<*>) = ap
  {-# INLINE liftA2 #-}
  liftA2 = liftM2

instance Monad Parser where
  {-# INLINE (>>=) #-}
  Parser pa >>= fpb = Parser $ \k s0 ok err -> pa k s0 (\a s1 -> let Parser pb = fpb a in pb k s1 ok err) err

instance Alternative Parser where
  {-# INLINE empty #-}
  empty = Parser $ \_ _ _ err -> err
  {-# INLINE (<|>) #-}
  Parser pa <|> Parser pb = Parser $ \k s ok err -> pa k s ok (pb k s ok err)

{-# INLINE char #-}
char :: Parser Char
char = Parser $ \k s0 ok err ->
  let go s = case k s of
        Done -> err
        Skip s' -> go s'
        Yield c s' -> ok c s'
   in go s0

{-# INLINE digit #-}
digit :: Parser Int
digit =
  char >>= \c ->
    if isDigit c
      then pure $ ord c - ord '0'
      else empty

expectChar :: Char -> Parser ()
expectChar c = char >>= \c' -> unless (c == c') empty

{-# INLINE p2digits #-}
p2digits :: Parser Int
p2digits = do
  d1 <- digit
  d2 <- digit
  pure $ d1 * 10 + d2

{-# INLINE p4digits #-}
p4digits :: Parser Int
p4digits = do
  d1 <- digit
  d2 <- digit
  d3 <- digit
  d4 <- digit
  pure $ d1 * 1000 + d2 * 100 + d3 * 10 + d4

{-# INLINE p12digits #-}
p12digits :: Parser Int
p12digits = do
  d1 <- digit
  md2 <- optional digit
  pure $ maybe d1 (\d2 -> d1 * 10 + d2) md2

parsePartialDate :: Parser PartialDate
parsePartialDate =
  asum
    [ do
        year <- p4digits <|> ((2000 +) <$> p2digits)
        sep
        month <- p12digits
        sep
        day <- p12digits
        maybe empty (pure . Exact) $ fromGregorianValid (fromIntegral year) month day,
      do
        d1 <- p12digits
        md2 <- optional $ sep >> p12digits
        pure $ case md2 of
          Nothing -> Partial Nothing d1
          Just d2 -> Partial (Just d1) d2
    ]
  where
    {-# INLINE sep #-}
    sep =
      char >>= \case
        '-' -> pure ()
        '/' -> pure ()
        _ -> empty

parseTime :: Parser (TimeOfDay, Maybe TimeZone)
parseTime = do
  tod <- do
    h <- p12digits
    expectChar ':'
    m <- p2digits
    maybe empty pure $ makeTimeOfDayValid h m 0
  mtz <- optional $ do
    expectChar ' '
    sign <-
      char >>= \case
        '+' -> pure 1
        '-' -> pure (-1)
        _ -> empty
    hmin <- (* 60) <$> p12digits
    mmin <- optional $ do
      expectChar ':'
      p2digits
    pure $ minutesToTimeZone $ sign * (hmin + fromMaybe 0 mmin)
  pure (tod, mtz)

parsePartialDateTime :: Parser (Either PartialDate PartialDateTime)
parsePartialDateTime =
  asum
    [ (\(tod, mtz) -> Right $ PartialDateTime tod Nothing mtz) <$> parseTime,
      do
        dt <- parsePartialDate
        mt <- optional $ expectChar ' ' >> parseTime
        pure $ case mt of
          Nothing -> Left dt
          Just (tod, mtz) -> Right $ PartialDateTime tod (Just dt) mtz
    ]

parseTimeStamp :: Parser (Timestamp PartialDate PartialDateTime)
parseTimeStamp = do
  start <- parsePartialDateTime
  mend <- optional $ do
    expectChar ' '
    expectChar '-'
    expectChar ' '
    parsePartialDateTime
  case (start, mend) of
    (Left dt, Nothing) -> pure $ Date dt
    (Right dt, Nothing) -> pure $ DateTime dt
    (Left start', Just (Left end)) -> pure $ DateRange start' end
    (Right start', Just (Right end)) -> pure $ DateTimeRange start' end
    _ -> empty

parseTimeStamps :: Text -> [Timestamp PartialDate PartialDateTime]
parseTimeStamps txt = case stream txt of
  Stream k s0 _ -> go s0
    where
      go s = case k s of
        Done -> []
        Skip s' -> go s'
        Yield '[' s' -> pTimestamp' k s' (\ts s'' -> ts : go s'') (go s')
        Yield _ s' -> go s'
  where
    Parser pTimestamp' = parseTimeStamp <* expectChar ']'
