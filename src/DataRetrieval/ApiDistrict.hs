{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataRetrieval.ApiDistrict where

import DataRetrieval.AesonUtil
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import GHC.Generics

type DistrictKey = Text

data ApiDistrict = ApiDistrict
  { ags :: DistrictKey,
    name :: Text,
    county :: Text,
    state :: Text,
    stateAbbreviation :: Text,
    population :: Int,
    cases :: Int,
    deaths :: Int,
    casesPerWeek :: Int,
    deathsPerWeek :: Int,
    recovered :: Int,
    weekIncidence :: Double,
    casesPer100k :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show)

parseDistrictList :: L.ByteString -> Maybe [ApiDistrict]
parseDistrictList input = do
  obj <- decode input :: Maybe Value
  let maybeHashList = parseDistrictList' obj
  case maybeHashList of
    Nothing -> Nothing
    (Just (HashList as)) -> Just as

parseDistrictList' :: Value -> Maybe (JSONHashList ApiDistrict)
parseDistrictList' = parseMaybe $
  withObject "<fields>" $ \obj -> do
    dataF <- obj .: "data"
    parseJSON dataF
