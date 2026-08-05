module Parser (loadLedger, parseDay) where
import Queries (normalizeLedger)
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
  return $ TimeOfDay h m (fromIntegral s)


parseDay :: Parser Day
parseDay = do
  d <- digitsN 2
  _ <- char '-'
  m <- digitsN 2
  _ <- char '-'
  y <- digitsN 4
  return $ fromGregorian (fromIntegral y) m d


parseTimestamp :: Parser LocalTime
parseTimestamp = LocalTime <$> parseDay <* char ' ' <*> parseTime

parseMoney :: Parser Money
parseMoney = do
    sign   <- option id (char '-' >> return negate)
    major <- many1 digit
    _ <- char '.'
    minor <- many1 digit
    return $ sign $ Money (read major) (read minor)

getAccountNames :: Parser [String]
getAccountNames = do 
    segment <- many1 alphaNum
    rest <- (char ':' >> getAccountNames) <|> return []
    return $ segment : rest


mkAccount :: [String] -> Money -> [Account]
mkAccount [] _ = []
mkAccount names balance = [Account (head names) balance subAccount]
    where  
        subAccount = mkAccount (tail names) balance


parseAccount :: Parser Account
parseAccount = do
    names <- getAccountNames
    skipMany1 space
    value <- parseMoney
    return $ head $ mkAccount names value

parseEntry :: Parser Entry
parseEntry = do
    tm <- parseTimestamp
    skipMany1 space
    title <- manyTill anyChar newline
    skipMany1 space
    a1 <- parseAccount
    skipMany1 space
    a2 <- parseAccount
    skipMany space
    return $ Entry tm title a1 a2


data LedgerError = ParserError ParseError | ConsistencyErr String deriving Show
consistencyCheck :: Accounts -> Either LedgerError Accounts
consistencyCheck accs
        | total == Money 0 0 = Right accs
        | otherwise = Left $ ConsistencyErr $ "Ledger does not sum to 0, total sum: " ++ show total
    where
        total = sum [balance x | x <- accs]


parseLedger :: String -> String -> Either LedgerError Ledger
parseLedger fileName contents = 
    case parse (many parseEntry) fileName contents of
         Left err -> Left $ ParserError err
         Right ledger -> Right ledger
 

loadLedger :: String -> String -> Either LedgerError (Ledger, Accounts)
loadLedger fileName contents = do
    ledger <- parseLedger fileName contents
    accounts <- consistencyCheck $ normalizeLedger ledger
    return (ledger, accounts)

