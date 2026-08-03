module Main where
import System.Environment
import Parser (loadLedger)
import qualified Data.Map as Map
import Data.Maybe 
import Data.Map (Map)
import Data.List (foldl', isPrefixOf)
import Data.Time.LocalTime (LocalTime(..))
import Data.Time.Calendar (dayPeriod)
import Data.Time.Calendar.Month (Month)
import Types


monthOf :: LocalTime -> Month
monthOf = dayPeriod . localDay

categoryPerMonth :: Ledger -> Map (String, Month) Money -> Map (String, Month) Money
categoryPerMonth entries initialBalance = foldl' step initialBalance entries
    where
        step accBalance entr = insertExpense (acc1 entr) (insertExpense (acc2 entr) accBalance)
            where
                insertExpense acc = Map.insertWith (+) (accountName acc, monthOf (timestamp entr)) (balance acc)


expensePerMonth :: Ledger -> [(Month, Money)]
expensePerMonth ledger =
        [(month, value) | ((_, month), value) <- filter (\((c, _), _) -> isPrefixOf "Expenses" c) categories]
    where
        categories = Map.toList $ categoryPerMonth ledger Map.empty


printExpensesPerMonth :: Ledger -> IO ()
printExpensesPerMonth ledger =
     putStrLn "Expenses per month: " >> mapM_ printEntry (expensePerMonth ledger)
  where
    printEntry (month, value) = putStrLn (show month ++ ": " ++ show value)

data PresentationMode = CSV | Console 
    deriving Show

data Flags
    = ShowExpensesPerMonth -- | -eM flag
    | ShowExpensesPerYear -- | -eY flag
    | TotalSpendingOn String -- | -t
    | CompareIncomeAndExpenses -- | -incomeExpense
    | AccountBalanceOverMonths -- | -bM
    | AccountBalanceOverYears -- | -bY
    | ShowTansactionsBetween LocalTime LocalTime -- TODO
    deriving Show

data ProgContext = ProgContext {
        progFlags :: [Flags],
        ledgerLoc :: String,
        presentationMode :: PresentationMode
    } deriving Show


processFlags :: [String] -> [Flags]
processFlags [] = []
processFlags (a:args) 
    | a == "-eM" = ShowExpensesPerMonth : processFlags args
    | a == "-eY" = ShowExpensesPerYear : processFlags args
    | a == "-t" = TotalSpendingOn (head args) : processFlags (tail args)
    | a == "-incomeExpense" = CompareIncomeAndExpenses : processFlags args
    | a == "-bM" = AccountBalanceOverMonths : processFlags args
    | a == "-bY" = AccountBalanceOverYears : processFlags args
    | otherwise = error ("Unkown argument: " ++ a)


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
    return (ProgContext flags loc Console)


main :: IO ()
main = do
    context <- prepareProg
    let fileName = ledgerLoc context
    contents <- readFile fileName
    case loadLedger fileName contents of
         Left err -> print err
         Right (ledger, _) -> print context

