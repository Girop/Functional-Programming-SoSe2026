module Main where
import Parser (loadLedger)
import Data.Map (Map, unionWith, insertWith, empty)
import Data.Time.LocalTime (LocalTime(..))
import Data.Time.Calendar (dayPeriod)
import Data.Time.Calendar.Month (Month)
import Types


monthOf :: LocalTime -> Month
monthOf = dayPeriod . localDay

expensesPerMonth :: Ledger -> Map (String, Month) Money -> Map (String, Month) Money
expensesPerMonth [] _ = empty
expensesPerMonth (entr: entrs) accBalance = expensesPerMonth entrs mergeExpenses
    where
        insertExpense acc = insertWith (+) (accountName acc, monthOf (timestamp entr)) (balance acc) accBalance
        mergeExpenses = unionWith (+) (insertExpense (acc1 entr)) (insertExpense (acc2 entr))
    

main :: IO ()
main = do
    let fileName = ".ledger"
    contents <- readFile fileName
    case loadLedger fileName contents of
         Left err -> print err
         Right _ -> print "Ledger loaded sucessfully"

