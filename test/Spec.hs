{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Text (Text, unpack)
import Data.Time
import Monolog.Time
import Test.Hspec

strRange :: Timestamp String String -> Timestamp Day ZonedTime
strRange ts = case ts of
  Date str -> Date (date str)
  DateTime str -> DateTime (datetime str)
  DateRange a b -> DateRange (date a) (date b)
  DateTimeRange a b -> DateTimeRange (datetime a) (datetime b)
  where
    date :: String -> Day
    date = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"
    datetime :: String -> ZonedTime
    datetime = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M %Ez"

instance Eq ZonedTime where
  ZonedTime a b == ZonedTime c d = a == c && b == d

hasTimestamps :: (HasCallStack) => Text -> [Timestamp String String] -> Spec
hasTimestamps txt expect = it (unpack txt) $ shouldBe (fromPartialTimestamp ref <$> parseTimeStamps txt) (strRange <$> expect)
  where
    ref = parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M %Ez" "2011-12-13 14:15 +09:00"

main :: IO ()
main =
  hspec $ do
    describe "timestamp parsing" $ do
      it "no timestamp" $ parseTimeStamps "foo bar" `shouldBe` []
      it "exact date" $
        shouldBe
          (parseTimeStamps "foo [2024-02-13] bar")
          [Date (Exact $ fromGregorian 2024 2 13)]
      it "exact date range" $
        shouldBe
          (parseTimeStamps "foo [2024-02-13 - 2024-02-14] bar")
          [DateRange (Exact $ fromGregorian 2024 2 13) (Exact $ fromGregorian 2024 2 14)]
      it "exact datetime" $
        shouldBe
          (parseTimeStamps "foo [2024-02-13 13:00] bar")
          [DateTime $ PartialDateTime (TimeOfDay 13 0 0) (Just (Exact $ fromGregorian 2024 2 13)) Nothing]
      it "time only" $
        shouldBe
          (parseTimeStamps "foo [13:00] bar")
          [DateTime $ PartialDateTime (TimeOfDay 13 0 0) Nothing Nothing]
      it "two exact dates" $
        shouldBe
          (parseTimeStamps "foo [2024-02-13] bar [2024-02-11] baz")
          [Date (Exact $ fromGregorian 2024 2 13), Date (Exact $ fromGregorian 2024 2 11)]
    describe "timestamp conversion" $ do
      hasTimestamps
        "foo [13:00 - 14:00] bar"
        [DateTimeRange "2011-12-13 13:00 +09:00" "2011-12-13 14:00 +09:00"]
      hasTimestamps
        "foo [3/14 13:00 - 01:00] bar"
        [DateTimeRange "2012-03-14 13:00 +09:00" "2012-03-14 14:00 +09:00"]

-- it "single timestamp" $ parseTimeStamps "[12 13:00 - 14 13:00]" `shouldBe` []
