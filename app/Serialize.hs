module Serialize where
import Types
import Data.List (intercalate)


class Serializable t where
    toCsv :: t -> String
    -- TODO: toJson :: t -> String


instance Serializable Ledger where 

    toCsv ledger = header ++ intercalate "\n" body
        where 
            header = "timestamp, name, account1, change1, account2, change2\n"
            body = formatEntry <$> ledger
            formatEntry e = intercalate ", " [show (timestamp e), title e, formatAccount (acc1 e), formatAccount (acc2 e)]
            formatAccount a = fullAccountName a ++ ", " ++ show (balance a)


instance Serializable Accounts where 
    toCsv accounts = header ++ body accounts
        where 
            header = "name, balance\n"
            body [] = ""
            body (a:as) = showEntry ++ body as
                where
                    showEntry = fullAccountName a ++ ", " ++ show (balance a) ++ "\n"

