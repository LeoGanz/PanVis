{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataRetrieval.HistoryUpdate
  ( parseHistoryUpdate,
    processHistoryUpdates,
    HistoryUpdate (..)
  )
where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import Data.List (transpose)
import Data.Text (Text)
import DataRetrieval.AesonUtil
import DataRetrieval.ApiDistrict (ApiDistrictKey)
import DataRetrieval.ApiHistoryData
import GHC.Generics (Generic)

data HistoryUpdate = HistoryUpdate
  { ags :: ApiDistrictKey,
    name :: Text,
    history :: [HistoryFragment]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

parseHistoryUpdate :: L.ByteString -> Maybe [HistoryUpdate]
parseHistoryUpdate input = do
  obj <- decode input :: Maybe Value
  let maybeHashList = parseHistoryUpdate' obj
  case maybeHashList of
    Nothing -> Nothing
    (Just (HashList histUpdates)) -> Just histUpdates

parseHistoryUpdate' :: Value -> Maybe (JSONHashList HistoryUpdate)
parseHistoryUpdate' = parseMaybe $
  withObject "<fields>" $ \obj -> do
    dataF <- obj .: "data"
    parseJSON dataF

-- assume correct order for simplicity
processHistoryUpdates :: [HistoryUpdate] -> [[HistoryFragment]]
processHistoryUpdates histUpdates = transpose $ map history histUpdates
