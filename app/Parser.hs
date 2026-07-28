module Parser (parseLedger) where

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Text.Parsec
import Text.Parsec.String (Parser)
import Types 


digitsN :: Int -> Parser Int
digitsN n = read <$> count n digit

parseTime :: Parser TimeOfDay
parseTime = do
  h <- digitsN 2
  _ <- char ':'
  m <- digitsN 2
  _ <- char ':'
  s <- digitsN 2
  return (TimeOfDay h m (fromIntegral s))


parseDay :: Parser Day
parseDay = do
  d <- digitsN 2
  _ <- char '-'
  m <- digitsN 2
  _ <- char '-'
  y <- digitsN 4
  return (fromGregorian (fromIntegral y) m d)


parseTimestamp :: Parser LocalTime
parseTimestamp = LocalTime <$> parseDay <* char ' ' <*> parseTime

signedInt :: Parser Int
signedInt = do
    sign   <- option id (char '-' >> return negate)
    digits <- many1 digit
    return $ sign (read digits)

parseFlow :: Parser Flow
parseFlow = do
    dirraw <- manyTill anyChar (char ':')
    dir <- case dirraw of 
        "Expenses"  -> return Outcoming
        "Income" -> return Incoming
        other -> fail ("Unkown money flow direction: " ++ other)
    category <- manyTill anyChar space
    skipMany1 space
    change <- signedInt
    return (Flow dir category change)


parseAssets :: Parser Asset
parseAssets = do
    assetTypeRaw <- manyTill anyChar (char ':')
    assetTypeName <- case assetTypeRaw of
        "Assets" -> return Cash
        _ -> fail "unexpected asset type"
    assetName <- manyTill anyChar space
    skipMany1 space
    value <- signedInt
    return (Asset assetTypeName assetName value)


parseEntry :: Parser Entry
parseEntry = do
    tm <- parseTimestamp
    skipMany1 space
    title <- manyTill anyChar newline
    skipMany1 space
    flow <- parseFlow
    skipMany1 space
    asset <- parseAssets
    skipMany space
    return (Entry tm title flow asset)

parseLedger :: String -> String -> Either ParseError Ledger
parseLedger fileName contents = parse (many parseEntry) fileName contents


