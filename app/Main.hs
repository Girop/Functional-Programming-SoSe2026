-- |
-- Module : Main
-- Description : Main machinery of the bookkeeping system
--
-- All of the modules are used here, inputs from the user are collected, queries are run in correct order 
-- and results are displayed in desired format.
module Main where

import Args
import Data.List (intercalate)
import Data.Maybe
import Parser (loadLedger)
import Queries
import System.Environment
import Types

-- | Read the desired ledger file location from ENV variables, otherwise default to ".ledger"
lookupLedgerLocation :: IO String
lookupLedgerLocation =
  unwrap <$> lookupEnv "LEDGER_FILE"
  where
    unwrap = Data.Maybe.fromMaybe ".ledger"

-- | Prepare all of the required input data for the system to run
prepareProg :: IO ProgContext
prepareProg = do
  loc <- lookupLedgerLocation
  args <- getArgs
  let flags = processFlags args
  return $ ProgContext flags loc

-- | Associate `QueryFlag`s with their respective queries.
query :: QueryFlag -> Ledger -> QueryResult
query ShowExpensesPerMonth = expensePerMonthQuery
query ShowExpensesPerYear = expensePerYearQuery
query AccountBalanceOverMonths = balanceOverMonthsQuery
query AccountBalanceOverYears = balanceOverYearsQuery
query AccountBalanceNow = balanceNowQuery

-- | Run all operations in the order of:
-- |    ledger -> filters -> queries
-- | Returns list of the query results.
runQueries :: [Flag] -> Ledger -> [QueryResult]
runQueries flags ledger = query' (foldl filteredLedger ledger flags) (getQueries flags)
  where
    filteredLedger ledger' f
      | FF (TransactionsBetween d0 d1) <- f = restrictTimeRange d0 d1 ledger'
      | otherwise = ledger'
    query' ledger' (f : fs) = query f ledger' : query' ledger' fs
    query' _ [] = []

-- | Interface for saving query results in form of .csv files.
-- | Each query will be saved to a file named "query_result{idx}.csv" (example: query_result0.csv")
-- | File index corresponds to the order of provided query flags.
dumpToCsv :: [QueryResult] -> IO ()
dumpToCsv qs = mapM_ (uncurry dumpSingleCSV) (zip csvFilenames qs)
  where
    csvFilenames = ["query_result" ++ show idx ++ ".csv" | idx <- [0 :: Integer ..]]
    dumpSingleCSV filename q = writeFile filename (header ++ "\n" ++ body)
      where
        header = intercalate "," (queryHeader q)
        body = intercalate "\n" $ intercalate "," <$> queryBody q

-- | Interface for showing query results in the console.
-- | Order of printing the results corresponds to to the order of provided query flags.
dumpToConsole :: [QueryResult] -> IO ()
dumpToConsole = mapM_ printQueryResult
  where
    printQueryResult q = do
      let hd = "|" ++ intercalate " | " (queryHeader q) ++ "|"
      let bar = replicate (length hd) '-'
      putStrLn bar
      putStrLn hd
      putStrLn bar
      putStrLn $ intercalate "|\n" $ intercalate " | " <$> queryBody q
      putStrLn "\n"

-- | Main intreface for bookkeeping system.
runProgram :: ProgContext -> Ledger -> IO ()
runProgram ctxt ledger = do
  let qs = runQueries (progFlags ctxt) ledger
  let ps = getPresentationMode (progFlags ctxt)
  matchMode ps qs
  where
    matchMode [] _ = return ()
    matchMode (p : ps) qs' = case p of
      CSVMode -> dumpToCsv qs' >> matchMode ps qs'
      ConsoleMode -> dumpToConsole qs' >> matchMode ps qs'

main :: IO ()
main = do
  context <- prepareProg
  let fileName = ledgerLoc context
  contents <- readFile fileName
  case loadLedger fileName contents of
    Left err -> putStrLn "While loading the ledger, following error has occured: " >> print err
    Right ledger -> runProgram context ledger
