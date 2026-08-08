-- |
-- Module : Queries
-- Description : Operations on ledgers
--
-- Here are defined all relevant for the user queries on ledger.
-- `QueryResult` is used to allow end-user of this module to display outputs in various tabular formats with ease.
module Queries
  ( QueryResult (..),
    restrictTimeRange,
    expensePerMonthQuery,
    expensePerYearQuery,
    balanceOverMonthsQuery,
    balanceOverYearsQuery,
    balanceNowQuery,
    normalizeLedger,
  )
where

import Data.List (foldl', isPrefixOf, partition, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time.Calendar (Day, dayPeriod, toGregorian)
import Data.Time.Calendar.Month (Month)
import Data.Time.Calendar.OrdinalDate (Year)
import Data.Time.LocalTime (LocalTime (..))
import Types

-- | Basic building block of the query system.
-- | Header values are column names
-- | Body values are rows with entries.
data QueryResult = QueryResult
  { queryHeader :: [String],
    queryBody :: [[String]]
  }

-- | A helper function to recursively merge flat list of accounts into a recursive strucutre with all subaccounts merged.
mergeAccounts :: Accounts -> Accounts
mergeAccounts [] = []
mergeAccounts (a : as) = mkAcc : mergeAccounts rest
  where
    (matchingAccs, rest) = partition (\other -> accountName other == accountName a) as
    accBalance = foldl' (\acc ac -> acc + balance ac) (balance a) matchingAccs
    mergedSubAccounts = mergeAccounts (subAccounts a ++ concatMap subAccounts matchingAccs)
    mkAcc = Account (accountName a) accBalance mergedSubAccounts

-- | Given a list of entries, calculate balance of each account after all of the operations.
-- | Assumes inital balance of all accounts to be 0
normalizeLedger :: Ledger -> Accounts
normalizeLedger ledger = mergeAccounts [accs | x <- ledger, accs <- [acc1 x, acc2 x]]

-- | Filter out all values which do not fall into the specified range
restrictTimeRange :: Day -> Day -> Ledger -> Ledger
restrictTimeRange d1 d2 = filter (\e -> let d0 = localDay (timestamp e) in d1 <= d0 && d0 <= d2)

-- restrictAccountNames :: [String] -> Ledger -> Ledger
-- restrictAccountNames names = filter ()
--   where
--     contains n = any (`elem` n) names
--     combineNames e = [accountName (acc1 e), accountName (acc2 e)]

-- | Helper month accessor
monthOf :: LocalTime -> Month
monthOf = dayPeriod . localDay

-- | Helper year accessor
yearOf :: LocalTime -> Year
yearOf t = let (y, _, _) = toGregorian (localDay t) in y

categoryPerTimePeriod :: (Ord a) => (LocalTime -> a) -> Ledger -> Map (String, a) Money -> Map (String, a) Money
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

groupBySelector :: (Ord k) => (a -> k) -> [a] -> Map k [a]
groupBySelector f = Map.fromListWith (++) . map (\x -> (f x, [x]))

timeperiodEntries :: (Ord a) => (LocalTime -> a) -> Ledger -> [(a, Ledger)]
timeperiodEntries timeSelector l = Map.toList $ groupBySelector (timeSelector . timestamp) l

balanceOverTimePeriod :: (Show a, Ord a) => (LocalTime -> a) -> Ledger -> QueryResult
balanceOverTimePeriod selector l = QueryResult h b
  where
    h = ["Time", "Account", "Balance"]
    periods = sortOn fst (timeperiodEntries selector l)
    cumulativePeriods = scanl1 (\(_, es1) (t2, es2) -> (t2, es1 ++ es2)) periods
    b = [[show t, accountName a, show (balance a)] | (t, es) <- cumulativePeriods, a <- normalizeLedger es]

balanceOverMonthsQuery :: Ledger -> QueryResult
balanceOverMonthsQuery = balanceOverTimePeriod monthOf

balanceOverYearsQuery :: Ledger -> QueryResult
balanceOverYearsQuery = balanceOverTimePeriod yearOf

balanceNowQuery :: Ledger -> QueryResult
balanceNowQuery l = QueryResult h b
  where
    h = ["Account", "Balance"]
    b = [[accountName a, show (balance a)] | a <- normalizeLedger l]
