{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Monolog.Parser
  ( parseTimeStamps,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Text (Text)
import Data.Text.Internal.Fusion
import Data.Time
import Monolog.DateTime

parseTimestamp :: Parser PartialTimestamp
parseTimestamp =
  -- TODO optimize
  asum
    [ do
        dt <- pPartialDate
        asum
          [ do
              sep
              end <- pPartialDate
              pure $ DateRangePartial dt end,
            do
              expectChar ' '
              tod <- pTimeOfDay
              asum
                [ do
                    sep
                    mEnd <- optional $ pPartialDate <* expectChar ' '
                    todEnd <- pTimeOfDay
                    mtz <- pMtz
                    pure $ DateTimeRangePartial dt tod mEnd todEnd mtz,
                  do
                    mtz <- pMtz
                    pure $ DateTimePartial dt tod mtz
                ],
            pure $ DatePartial dt
          ],
      do
        dt <- pFullDate
        asum
          [ do
              sep
              end <- pFullDate
              pure $ DateRangeFull dt end,
            do
              expectChar ' '
              tod <- pTimeOfDay
              asum
                [ do
                    sep
                    mEnd <- optional $ pFullDate <* expectChar ' '
                    todEnd <- pTimeOfDay
                    mtz <- pMtz
                    pure $ DateTimeRangeFull dt tod mEnd todEnd mtz,
                  do
                    mtz <- pMtz
                    pure $ DateTimeFull dt tod mtz
                ],
            pure $ DateFull dt
          ],
      do
        tod1 <- pTimeOfDay
        asum
          [ do
              sep
              tod2 <- pTimeOfDay
              mtz <- pMtz
              pure $ DateTimeRangeToday tod1 tod2 mtz,
            do
              mtz <- pMtz
              pure $ DateTimeToday tod1 mtz
          ]
    ]
  where
    pMtz = optional $ expectChar ' ' >> pTimeZone
    sep = expectChar ' ' >> expectChar '-' >> expectChar ' '

pPartialDate :: Parser PartialDate
pPartialDate = do
  m <- p12digits
  guard $ m > 0 && m <= 12
  expectChar '/'
  d <- p12digits
  guard $ d > 0 && d < 32
  pure $ PartialDate m d

pFullDate :: Parser Day
pFullDate = do
  y <- p4digits
  expectChar '-'
  m <- p2digits
  expectChar '-'
  d <- p2digits
  maybe empty pure $ fromGregorianValid (fromIntegral y) m d

pTimeOfDay :: Parser TimeOfDay
pTimeOfDay = do
  h <- p12digits
  guard $ h >= 0 && h < 23
  expectChar ':'
  m <- p2digits
  guard $ m >= 0 && m < 59
  pure $ TimeOfDay h m 0

pTimeZone :: Parser TimeZone
pTimeZone = do
  sign <-
    char >>= \case
      '+' -> pure 1
      '-' -> pure (-1)
      _ -> empty
  h <- p12digits
  m <- optional $ do
    expectChar ':'
    m <- p2digits
    guard $ m >= 0 && m < 59
    pure m
  pure $ minutesToTimeZone (sign * (h * 60 + fromMaybe 0 m))

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

parseTimeStamps :: Text -> [PartialTimestamp]
parseTimeStamps txt = case stream txt of
  Stream k s0 _ -> go s0
    where
      go s = case k s of
        Done -> []
        Skip s' -> go s'
        Yield '[' s' -> pTimestamp' k s' (\ts s'' -> ts : go s'') (go s')
        Yield _ s' -> go s'
  where
    Parser pTimestamp' = parseTimestamp <* expectChar ']'
