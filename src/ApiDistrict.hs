{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiDistrict where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
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

parseDistrictList :: L.ByteString -> Maybe (JSONHashList ApiDistrict)
parseDistrictList input = do
  obj <- decode input :: Maybe Value
  parseDistrictList' obj

parseDistrictList' :: Value -> Maybe (JSONHashList ApiDistrict)
parseDistrictList' = parseMaybe $
  withObject "<fields>" $ \obj -> do
    dataF <- obj .: "data"
    parseJSON dataF

-- code below is adapted from https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

newtype JSONHashList a = HashList [a]
  deriving (Show)

instance FromJSON a => FromJSON (JSONHashList a) where
  parseJSON = withObject "JSONHashList" $ \obj ->
    let kvs = HM.toList obj
        values = map snd kvs
        parsed = mapM parseJSON values
     in fmap HashList parsed
