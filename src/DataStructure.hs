module DataStructure where

import Control.Monad (join)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import DataRetrieval.ApiDataManager
import DataRetrieval.ApiDistrict (ags, name, population)
import Util

type CountryKey = Text -- abbreviations like DE

type StateKey = Text -- abbreviations like BY

type DistrictKey = Text -- AGS (allgemeiner Gemeindeschlüssel), e.g. 09162 for Munich (text instead of ints because of leading zeros)

type States = Map StateKey State

type Districts = Map DistrictKey District

data Country = Country
  { _countryName :: Text,
    _countryKey :: CountryKey,
    _states :: States
  }
  deriving (Show)

data State = State
  { _stateName :: Text,
    _stateKey :: StateKey,
    _districts :: Districts
  }
  deriving (Show)

data District = District
  { _districtName :: Text,
    _districtKey :: DistrictKey,
    _population :: Int,
    _weekIncidence :: Double
  }
  deriving (Show)

-- result is IO as some information is fetched over http.
-- result is Nothing if something went wrong with the http request or parsing or if no information was found for the specified district key.
buildDistrict :: DistrictKey -> Double -> IO (Maybe District)
buildDistrict key incidence = do
  apiDistrict <- join <$> find ((== key) . ags) <<$>> getDistrictsList
  let distNameMay = name <$> apiDistrict
      distPopMay = population <$> apiDistrict
  case distNameMay of
    Nothing -> return Nothing
    Just distName -> case distPopMay of
      Nothing -> return Nothing
      Just distPop -> return $ Just District {_districtName = distName, _districtKey = key, _population = distPop, _weekIncidence = incidence}
