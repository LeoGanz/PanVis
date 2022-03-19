{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DataStructure where

import Control.Monad (join)
import Data.Function (on)
import Data.List (find, groupBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import DataRetrieval.ApiDataManager
import DataRetrieval.ApiDistrict (ags, name, population)
import History
import Lens.Micro.Platform
import Util

type DistrictKey = Text -- AGS (allgemeiner Gemeindeschlüssel), e.g. 09162 for Munich (text instead of ints because of leading zeros)

data District = District
  { _districtName :: Text,
    _districtKey :: DistrictKey,
    _population :: Int,
    _weekIncidence :: Double
  }
  deriving (Show)

type Districts = Map DistrictKey District

makeLenses ''District

type StateKey = Text -- abbreviations like BY

data State = State
  { _stateName :: Text,
    _stateKey :: StateKey,
    _districts :: Districts
  }
  deriving (Show)

type States = Map StateKey State

makeLenses ''State

type CountryKey = Text -- abbreviations like DE

data Country = Country
  { _countryName :: Text,
    _countryKey :: CountryKey,
    _states :: States
  }
  deriving (Show)

makeLenses ''Country

-- result is IO as some information is fetched over http.
-- result is Nothing if something went wrong with the http request or parsing or if no information was found for the specified district key.
buildDistrict :: DistrictKey -> Double -> IO (Maybe District)
buildDistrict key incidence = do
  apiDistrict <- join <$> find ((== key) . DataRetrieval.ApiDistrict.ags) <<$>> getDistrictsList
  let distNameMay = DataRetrieval.ApiDistrict.name <$> apiDistrict
      distPopMay = DataRetrieval.ApiDistrict.population <$> apiDistrict
  case distNameMay of
    Nothing -> return Nothing
    Just distName -> case distPopMay of
      Nothing -> return Nothing
      Just distPop -> return $ Just District {_districtName = distName, _districtKey = key, _population = distPop, _weekIncidence = incidence}

fromHistory :: History -> Day -> Maybe (Day, Country)
fromHistory (History (Header infos) (Body snapshots)) day = case snapshotMay of
  Nothing -> Nothing
  Just snapshot -> Just (day, buildCountry snapshot)
  where
    snapshotMay = find (\(Snapshot d _) -> day == d) snapshots
    getVals (Snapshot day vals) = vals
    build incidence info = (stateAbbreviation info, District {_districtName = pack (History.name info), _districtKey = pack (History.ags info), _population = History.population info, _weekIncidence = incidence})
    statekeyDistricts snapshot = zipWith build (getVals snapshot) infos -- :: [(statekey, district)]
    groupedDists snapshot = groupBy ((==) `on` fst) $ sortBy (comparing fst) $ statekeyDistricts snapshot
    buildStates snapshot = map (\all@((st, _) : _) -> State {_stateName = pack st, _stateKey = pack st, _districts = Map.fromList (map (\(_, dist) -> (_districtKey dist, dist)) all)}) $ groupedDists snapshot
    buildCountry snapshot = Country {_countryName = "Deutschland", _countryKey = "DE", _states = Map.fromList (map (\state -> (_stateKey state, state)) (buildStates snapshot))}
fromHistory _ _ = error "fromHistory only supported for default histories"
