module Main where
import Parser (parseLedger)
import Data.List (partition)
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


consistencyCheck :: Accounts -> Bool
consistencyCheck accs = sum [balance x | x <- accs] == (Money 0 0)

expensesPerMonth :: Accounts -> Map Month Integer
-- expensesPerMonth accs = -- TODO

displayAccounts :: Accounts -> IO ()
displayAccounts accounts = -- TODO
    

main :: IO ()
main = do
    let fileName = ".ledger"
    contents <- readFile fileName
    case parseLedger fileName contents of
         Left err -> print err
         Right xs -> print $ consistencyCheck $ normalizeLedger xs

