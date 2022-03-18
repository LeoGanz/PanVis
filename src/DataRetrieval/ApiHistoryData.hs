{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataRetrieval.ApiHistoryData
  ( HistoryFragment (..),
    parseDistrictHistory,
  )
where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import Data.Time
import DataRetrieval.AesonUtil
import DataRetrieval.ApiDistrict (ApiDistrictKey)
import GHC.Generics (Generic)

data HistoryFragment = HistoryFragment
  { weekIncidence :: Double,
    date :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

instance Ord HistoryFragment where
  compare (HistoryFragment w1 d1) (HistoryFragment w2 d2) = case compare d1 d2 of
    EQ -> compare w1 w2
    cmp -> cmp

parseDistrictHistory :: ApiDistrictKey -> L.ByteString -> Maybe [HistoryFragment]
parseDistrictHistory ags input = decode input >>= parseDistrictHistory' ags 

parseDistrictHistory' :: ApiDistrictKey -> Value -> Maybe [HistoryFragment]
parseDistrictHistory' ags = parseMaybe $
  withObject "<fields>" $ \obj -> do
    history <- obj .: "data" .-> ags .-> "history"
    parseJSONList history

