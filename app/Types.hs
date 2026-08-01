module Types where

import Data.Time.LocalTime (LocalTime (..))


data Money = Money {
        majorUnit :: Integer,
        minorUnit :: Integer
    } deriving (Show, Eq)


instance Num Money where
    (Money maj1 min1) + (Money maj2 min2) = 
        let minors = min1 + min2
        in Money (maj1 + maj2 + minors `div` 100) (minors `mod` 100)

    -- Why would you do that?
    (Money maj1 min1) * (Money maj2 min2) =
        (Money 0 0) + (Money (maj1 * maj2) (min1 * min2))

    abs (Money maj1 min1) = Money (abs maj1) (abs min1)

    signum (Money maj1 min1) = Money (signum maj1) (signum min1)
    
    fromInteger a = Money a 0

    negate (Money maj1 min1) = Money (-maj1) (-min1)


data Account = Account {
        accountName :: String,
        balance :: Money,
        subAccounts :: [Account]
    } deriving (Show, Eq)

data Entry = Entry { 
        timestamp :: LocalTime,
        title :: String,
        acc1 :: Account,
        acc2 :: Account
    } deriving (Show, Eq)

type Ledger = [Entry]
type Accounts = [Account]
