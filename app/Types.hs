module Types where

import Data.Time.LocalTime (LocalTime (..))


data FlowDirection = Incoming | Outcoming deriving Show
data Flow = Flow {
        flowDir :: FlowDirection,
        flowCategory :: String,
        flowChange :: Int
    } deriving Show


data AssetType = Cash deriving Show
data Asset = Asset {
      assetType :: AssetType,
      assetName :: String,
      assetChange :: Int
    } deriving Show

data Entry = Entry { 
        timestamp :: LocalTime,
        title :: String,
        flow :: Flow,
        asset :: Asset
    } deriving (Show)

type Ledger = [Entry]
