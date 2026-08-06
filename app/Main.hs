module Main where
import Data.List (intercalate)
import Data.Maybe
import Parser (loadLedger)
import Queries
import System.Environment
import Types
import Args


lookupLedgerLocation :: IO String
lookupLedgerLocation =
  unwrap <$> lookupEnv "LEDGER_FILE"
  where
    unwrap = Data.Maybe.fromMaybe ".ledger"


prepareProg :: IO ProgContext
prepareProg = do
  loc <- lookupLedgerLocation
  args <- getArgs
  let flags = processFlags args
  return $ ProgContext flags loc


query :: QueryFlag -> Ledger -> QueryResult
query ShowExpensesPerMonth = expensePerMonthQuery
query ShowExpensesPerYear = expensePerYearQuery
query AccountBalanceOverMonths = balanceOverMonthsQuery
query AccountBalanceOverYears = balanceOverYearsQuery
query AccountBalanceNow = balanceNowQuery


runQueries :: [Flag] -> Ledger -> [QueryResult]
runQueries flags ledger = query' (foldl filteredLedger ledger flags) (getQueries flags)
  where
    filteredLedger ledger' f
        | FF (TransactionsBetween d0 d1) <- f = restrictTimeRange d0 d1 ledger'
        | otherwise = ledger
    query' ledger' (f:fs) = query f ledger' : query' ledger' fs
    query' _ [] = []

dumpSingleCSV :: String -> QueryResult -> IO ()
dumpSingleCSV filename q = writeFile filename (header ++ "\n" ++ body)
  where
    header = intercalate "," (queryHeader q)
    body = intercalate "\n" $ intercalate "," <$> queryBody q


dumpToCsv :: [QueryResult] -> IO ()
dumpToCsv qs = mapM_ (uncurry dumpSingleCSV) (zip csvFilenames qs)
  where
    csvFilenames = ["query_result" ++ show idx ++ ".csv" | idx <- [0 :: Integer ..]]


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


runProgram :: ProgContext -> Ledger -> Accounts -> IO ()
runProgram ctxt ledger _ = do
    let qs = runQueries (progFlags ctxt) ledger
    let ps = getPresentationMode (progFlags ctxt)
    matchMode ps qs
    where
        matchMode [] _ = return ()
        matchMode (p:ps) qs' = case p of 
            CSVMode -> dumpToCsv qs' >> matchMode ps qs'
            ConsoleMode -> dumpToConsole qs' >> matchMode ps qs'


main :: IO ()
main = do
  context <- prepareProg
  let fileName = ledgerLoc context
  contents <- readFile fileName
  case loadLedger fileName contents of
    Left err -> putStrLn "While loading the ledger, following error occured: " >> print err
    Right (ledger, accounts) -> runProgram context ledger accounts
