module Queries (
    QueryResult(..),
    restrictTimeRange,
    expensePerMonthQuery,
    expensePerYearQuery,
    balanceOverMonthsQuery,
    balanceOverYearsQuery,
    balanceNowQuery,
    totalSpendingOnQuery,
    normalizeLedger
) where
import Data.Time.LocalTime (LocalTime(..))
import Data.Time.Calendar.OrdinalDate (Year)
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar (Day, dayPeriod, toGregorian)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl', isPrefixOf, partition)
import Types


mergeAccounts :: Accounts -> Accounts
mergeAccounts [] = []
mergeAccounts (a : as) = mkAcc : mergeAccounts rest
    where
        (matchingAccs, rest) = partition (\other -> accountName other == accountName a) as
        summedBalance = foldl (\acc ac -> acc + balance ac) (balance a) matchingAccs
        mergedSubAccounts = mergeAccounts (subAccounts a ++ concatMap subAccounts matchingAccs)
        mkAcc = Account (accountName a) summedBalance mergedSubAccounts


extractTransactionAccs :: Ledger -> Accounts
extractTransactionAccs ledger = [accs | x <- ledger, accs <- [acc1 x, acc2 x]]


normalizeLedger :: Ledger -> Accounts
normalizeLedger ledger = mergeAccounts $ extractTransactionAccs ledger


data QueryResult = QueryResult {
        queryHeader :: [String],
        queryBody :: [[String]]
    }


restrictTimeRange :: Day -> Day -> Ledger -> Ledger
restrictTimeRange d1 d2 = filter (\e -> let d0 = localDay (timestamp e) in d1 <= d0 && d0 <= d2)


monthOf :: LocalTime -> Month
monthOf = dayPeriod . localDay


yearOf :: LocalTime -> Year
yearOf t = let (y, _, _) = toGregorian (localDay t) in y


categoryPerTimePeriod :: Ord a => (LocalTime -> a) -> Ledger -> Map (String, a) Money -> Map (String, a) Money
categoryPerTimePeriod timeSelector entries initialBalance = foldl' step initialBalance entries
    where
        step accBalance entr = insertExpense (acc1 entr) (insertExpense (acc2 entr) accBalance)
            where
                insertExpense acc = Map.insertWith (+) (accountName acc, timeSelector (timestamp entr)) (balance acc)


categoryPerMonth :: Ledger -> Map (String, Month) Money
categoryPerMonth l = categoryPerTimePeriod monthOf l Map.empty


expensePerMonth :: Ledger -> [(Month, Money)]
expensePerMonth ledger =
        [(month, value) | ((_, month), value) <- filter (\((c, _), _) -> "Expenses" `isPrefixOf` c) categories]
    where
        categories = Map.toList $ categoryPerMonth ledger


categoryPerYear :: Ledger -> Map (String, Year) Money
categoryPerYear l = categoryPerTimePeriod yearOf l Map.empty


expensesPerYear :: Ledger -> [(Year, Money)]
expensesPerYear ledger =
        [(year, value) | ((_, year), value) <- filter (\((c, _), _) -> "Expenses" `isPrefixOf` c) categories]
    where
        categories = Map.toList $ categoryPerYear ledger


expensePerMonthQuery :: Ledger -> QueryResult
expensePerMonthQuery l = QueryResult h b
    where
        h = ["Month", "Expenses"]
        b = [[show m, show v] | (m, v) <- expensePerMonth l]


expensePerYearQuery :: Ledger -> QueryResult
expensePerYearQuery l = QueryResult h b
    where
        h = ["Year", "Expenses"]
        b = [[show y, show v] | (y, v) <- expensesPerYear l]


groupBySelector :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBySelector f = Map.fromListWith (++) . map (\x -> (f x, [x]))


timeperiodEntries :: Ord a => (LocalTime -> a) -> Ledger -> [(a, Ledger)]
timeperiodEntries timeSelector l = Map.toList $ groupBySelector (timeSelector . timestamp) l


balanceOverTimePeriod :: Show a => Ord a => (LocalTime -> a) -> Ledger -> QueryResult
balanceOverTimePeriod selector l = QueryResult h b
    where 
        h = ["Time", "Account", "Balance"]
        b = [[show tm, name, show money] | (tm, name, money) <- grouped]
        grouped = [(t, accountName a, balance a) | (t, es) <- timeperiodEntries selector l, a <- normalizeLedger es]


balanceOverMonthsQuery :: Ledger -> QueryResult
balanceOverMonthsQuery = balanceOverTimePeriod monthOf
    

balanceOverYearsQuery :: Ledger -> QueryResult
balanceOverYearsQuery = balanceOverTimePeriod yearOf


-- TODO
balanceNowQuery :: Ledger -> QueryResult
balanceNowQuery _ = QueryResult [] []

-- TODO
totalSpendingOnQuery :: String -> Ledger -> QueryResult
totalSpendingOnQuery _ _ = QueryResult [] []

