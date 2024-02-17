{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Maybe
import Data.Text (Text, unpack)
import Data.Time
import Monolog.DateTime
import Monolog.Parser
import Test.Hspec

parseDate :: String -> Day
parseDate = parseTimeOrError False defaultTimeLocale "%Y-%m%-d"

parseDateTime :: String -> ZonedTime
parseDateTime = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M %Ez"

date :: String -> Timestamp
date = Date . parseDate

dateRange :: String -> String -> Timestamp
dateRange s1 s2 = DateRange (parseDate s1) (parseDate s2)

datetime :: String -> Timestamp
datetime = DateTime . parseDateTime

datetimeRange :: String -> String -> Timestamp
datetimeRange s1 s2 = DateTimeRange (parseDateTime s1) (parseDateTime s2)

instance Eq ZonedTime where
  ZonedTime a b == ZonedTime c d = a == c && b == d

deriving instance Eq Timestamp

hasTimestamps :: (HasCallStack) => Text -> [Timestamp] -> Spec
hasTimestamps txt expect = it (unpack txt) $ shouldBe (mapMaybe (fromPartialTimestamp ref) $ parseTimeStamps txt) expect
  where
    ref = parseDateTime "2011-12-13 14:15 +09:00"

main :: IO ()
main =
  hspec $ do
    describe "timestamp parsing" $ do
      it "no timestamp" $ parseTimeStamps "foo bar" `shouldBe` []
      it "ToD range" $
        shouldBe
          (parseTimeStamps "foo [13:00 - 14:00] bar")
          [DateTimeRangeToday (TimeOfDay 13 0 0) (TimeOfDay 14 0 0) Nothing]
      it "partial DateTime range" $
        shouldBe
          (parseTimeStamps "foo [3/14 13:00 - 01:00] bar")
          [DateTimeRangePartial (PartialDate 3 14) (TimeOfDay 13 0 0) Nothing (TimeOfDay 1 0 0) Nothing]
    --   it "exact date" $
    --     shouldBe
    --       (parseTimeStamps "foo [2024-02-13] bar")
    --       [DateFull (fromGregorian 2024 2 13)]
    --   it "exact date range" $
    --     shouldBe
    --       (parseTimeStamps "foo [2024-02-13 - 2024-02-14] bar")
    --       [DateRange (Exact $ fromGregorian 2024 2 13) (Exact $ fromGregorian 2024 2 14)]
    --   it "exact datetime" $
    --     shouldBe
    --       (parseTimeStamps "foo [2024-02-13 13:00] bar")
    --       [DateTime $ PartialDateTime (TimeOfDay 13 0 0) (Just (Exact $ fromGregorian 2024 2 13)) Nothing]
    --   it "time only" $
    --     shouldBe
    --       (parseTimeStamps "foo [13:00] bar")
    --       [DateTime $ PartialDateTime (TimeOfDay 13 0 0) Nothing Nothing]
    --   it "two exact dates" $
    --     shouldBe
    --       (parseTimeStamps "foo [2024-02-13] bar [2024-02-11] baz")
    --       [Date (Exact $ fromGregorian 2024 2 13), Date (Exact $ fromGregorian 2024 2 11)]
    describe "timestamp conversion" $ do
      hasTimestamps
        "foo [13:00] bar"
        [datetime "2011-12-13 13:00 +09:00"]
      hasTimestamps
        "foo [13:00 +00:00] bar"
        [datetime "2011-12-13 13:00 +00:00"]
      hasTimestamps
        "foo [13:00 - 14:00] bar"
        [datetimeRange "2011-12-13 13:00 +09:00" "2011-12-13 14:00 +09:00"]
      hasTimestamps
        "foo [3/14 13:00] bar"
        [datetime "2012-03-14 13:00 +09:00"]
      hasTimestamps
        "foo [3/14 13:00 +0] bar"
        [datetime "2012-03-14 13:00 +00:00"]
      hasTimestamps
        "foo [3/14 13:00 - 01:00] bar"
        [datetimeRange "2012-03-14 13:00 +09:00" "2012-03-15 01:00 +09:00"]
      hasTimestamps
        "foo [2012-03-14 13:00 - 01:00] bar"
        [datetimeRange "2012-03-14 13:00 +09:00" "2012-03-15 01:00 +09:00"]
