-- |
-- Module : Args
-- Description : Console user interface 
--
-- Defines all of the flags which can be used by the user of the system.
-- Takes care of parsing and aggregating allowed options.
module Args
  ( Flag (..),
    QueryFlag (..),
    FilterFlags (..),
    PresentationMode (..),
    ProgContext (..),
    getQueries,
    getFilters,
    getPresentationMode,
    processFlags,
  )
where

import Data.List (nub)
import Data.Time.Calendar (Day)
import Parser (parseDay)
import Text.Parsec

-- | Specifies how to display results of a query.
-- | Multiple can be chosen, by default only `ConsoleMode` is chosen.
data PresentationMode = CSVMode | ConsoleMode
  deriving (Show, Eq)

-- | Turning on one of those flags result in analysis being made.
-- | Multiple flags can be turned on at the same time, respective CLI flags are written on top of each `QueryFlag`.
data QueryFlag
  = -- | -eM
    ShowExpensesPerMonth
  | -- | -eY
    ShowExpensesPerYear
  | -- | -bM
    AccountBalanceOverMonths
  | -- | -bY
    AccountBalanceOverYears
  | -- | -bN
    AccountBalanceNow
  deriving (Show, Eq)

-- | Use to filter out entries from the ledger which do not conform to specified criteria.
-- | All of those filter are applied before any of the queries.
data FilterFlags
  = -- | Specifiy the time range of ledger entries. Entries outside of this range are ignored.
    TransactionsBetween Day Day
  deriving (Show, Eq)

data Flag
  = QF QueryFlag
  | FF FilterFlags
  | PM PresentationMode
  deriving (Show, Eq)

type Flags = [Flag]

-- | All of the input information required for this program to correctly run.
data ProgContext = ProgContext
  { progFlags :: Flags,
    ledgerLoc :: String
  }

getQueries :: Flags -> [QueryFlag]
getQueries fs = [f | QF f <- fs]

getFilters :: Flags -> [FilterFlags]
getFilters fs = [f | FF f <- fs]

getPresentationMode :: Flags -> [PresentationMode]
getPresentationMode fs = [f | PM f <- fs]

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

-- | Main entry point for argument processing.
-- | Accepts command line input arguments and tries to match them  against predefined options.
-- | Collects all 3 types of flags and wrap them up in a general `Flag` type.
-- | Unrecognized argumnes are silently ignored.
processFlags :: [String] -> [Flag]
processFlags args = (QF <$> processQueryFlags args) ++ (FF <$> processFilterFlags args) ++ presModes
  where
    presModes =
      let modes = nub (PM <$> processModeFlags args)
       in if null modes then [PM ConsoleMode] else modes
