module Args where 
import Data.Time.Calendar (Day)
import Text.Parsec
import Parser (parseDay)
import Data.List (nub)


data PresentationMode = CSVMode | ConsoleMode
  deriving (Show, Eq)


data QueryFlag
  = ShowExpensesPerMonth
  | -- | -eM
    ShowExpensesPerYear
  | -- | -eY
    AccountBalanceOverMonths
  | -- | -bM
    AccountBalanceOverYears
  | -- | -bY
    AccountBalanceNow -- | -bN
  deriving ( Show, Eq)

data FilterFlags = TransactionsBetween Day Day
  deriving (-- | -t 02-01-2026 10-01-2026
            Show, Eq)

data Flag
  = QF QueryFlag
  | FF FilterFlags
  | PM PresentationMode
  deriving (Show, Eq)


type Flags = [Flag]


getQueries :: Flags -> [QueryFlag]
getQueries fs = [f | QF f <- fs]


getFilters :: Flags -> [FilterFlags]
getFilters fs = [f | FF f <- fs]


getPresentationMode :: Flags -> [PresentationMode]
getPresentationMode fs = [f | PM f <- fs]


data ProgContext = ProgContext
  { progFlags :: Flags,
    ledgerLoc :: String
  } deriving (Show)

processQueryFlags :: [String] -> [QueryFlag]
processQueryFlags [] = []
processQueryFlags (a : args)
  | a == "-eM" = ShowExpensesPerMonth : processQueryFlags args
  | a == "-eY" = ShowExpensesPerYear : processQueryFlags args
  | a == "-bM" = AccountBalanceOverMonths : processQueryFlags args
  | a == "-bY" = AccountBalanceOverYears : processQueryFlags args
  | a == "-bN" = AccountBalanceNow : processQueryFlags args
  | otherwise = processQueryFlags args

parseArgDay :: String -> Day
parseArgDay arg = case parse parseDay "<CLI>" arg of
  Left err -> error ("Invalid argument format provided" ++ show err)
  Right d -> d

processFilterFlags :: [String] -> [FilterFlags]
processFilterFlags [] = []
processFilterFlags (a : args)
  | a == "-t" = TransactionsBetween (parseArgDay (head args)) (parseArgDay (head (tail args))) : processFilterFlags (tail (tail args))
  | otherwise = processFilterFlags args

processModeFlags :: [String] -> [PresentationMode]
processModeFlags [] = []
processModeFlags (a : args)
  | a == "--csv" = CSVMode : processModeFlags args
  | a == "--console" = ConsoleMode : processModeFlags args
  | otherwise = processModeFlags args

processFlags :: [String] -> [Flag]
processFlags args = (QF <$> processQueryFlags args) ++ (FF <$> processFilterFlags args) ++ presModes
    where 
        presModes = let modes = nub (PM <$> processModeFlags args) in
            if null modes then [PM ConsoleMode] else modes
        
