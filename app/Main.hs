module Main where

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Text.Parsec
import Text.Parsec.String (Parser)

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
parseTimestamp = do
    d <- parseDay
    _ <- char ' '
    t <- parseTime
    return (LocalTime d t)


data FlowDirection = Incoming | Outcoming deriving Show

data Flow = Flow {
        flowDir :: FlowDirection,
        flowCategory :: String,
        flowChange :: Int
    } deriving Show

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


data AssetType = Cash deriving Show
data Asset = Asset {
      assetType :: AssetType,
      assetName :: String,
      assetChange :: Int
    } deriving Show


parseAssets :: Parser Asset
parseAssets = do
    assetTypeRaw <- manyTill anyChar (char ':')
    assetType <- case assetTypeRaw of
        "Assets" -> return Cash
        _ -> fail "unexpected asset type"
    assetName <- manyTill anyChar space
    skipMany1 space
    value <- signedInt
    return (Asset assetType assetName value)


data Entry = Entry { 
        timestamp :: LocalTime,
        title :: String,
        flow :: Flow,
        asset :: Asset
    } deriving (Show)


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


parseLedger :: String -> Either ParseError [Entry]
parseLedger contents = parse (many parseEntry) ".ledger" contents

    
main :: IO ()
main = do
    let fileName = ".ledger"
    contents <- readFile fileName
    case  parseLedger contents of
         Left err -> print err
         Right xs -> print xs

