{-# LANGUAGE OverloadedStrings #-}

module DataRetrieval.ApiDataManager where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import Data.List (transpose)
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import DataRetrieval.ApiAccess
import DataRetrieval.ApiDistrict (ApiDistrict, DistrictKey, ags, parseDistrictList)
import DataRetrieval.ApiHistoryData
import Debug.Trace (trace)
import Network.HTTP.Simple
import Safe (lastMay)

getDistrictsList :: IO (Maybe [ApiDistrict])
getDistrictsList = parseDistrictList . getResponseBody <$> httpLBS apiDistrictsRequest

historyIncidenceFile :: FilePath
historyIncidenceFile = "historyIncidence.csv"

timeFormat :: String
timeFormat = "%Y-%m-%d"

dayZero :: Day
dayZero = ModifiedJulianDay {toModifiedJulianDay = 0}

updateHistoryIncidenceFile :: IO ()
updateHistoryIncidenceFile = do
  distsM <- getDistrictsList
  case distsM of
    Nothing -> print "Error: Could not load districts data from Api"
    Just dists -> do
      let agss = map ags dists
      TIO.appendFile historyIncidenceFile ""
      contents <- readFile historyIncidenceFile
      let lastLine = lastMay $ lines contents
      case lastLine of
        Nothing -> do
          TIO.writeFile historyIncidenceFile $ T.append (T.intercalate ", " ("date" : agss)) "\n" -- header line of csv
          doUpdate agss dayZero -- fetch data from the beginning
        Just l -> do
          let lastUpdateMay = parseTimeM True defaultTimeLocale timeFormat (takeWhile (/= ',') l) :: Maybe Day
          case lastUpdateMay of
            Nothing -> doUpdate agss dayZero -- fetch data from the beginning
            Just lastUpdate -> do
              now <- utctDay <$> getCurrentTime
              when (addDays 1 lastUpdate < now) (doUpdate agss lastUpdate)

doUpdate :: [DistrictKey] -> Day -> IO ()
doUpdate agss lastUpdate = do
  --let agss = ["10042", "10043"] -- used for testing to have fewer api requests
  now <- utctDay <$> getCurrentTime
  let days = diffDays now lastUpdate
  print $ "fetching history data for the last " ++ show days ++ " days"

  histories <- sequence <$> mapM (fetchAndParse (fromInteger days)) agss
  --  mapM_ (mapM_ (print . date . head))  histories
  let firstDates = sequence $ mapM (map (utctDay . date . head)) histories
      firstDate = fromJust $ minimum <$> firstDates
      lastDates = sequence $ mapM (map (utctDay . date . last)) histories
      lastDate = fromJust $ maximum <$> lastDates
      preprocessedHistories = sequence $ mapM (map (preprocess firstDate lastDate)) histories
  let incidencesPerDay = transpose <$> preprocessedHistories
  case incidencesPerDay of
    Nothing -> print "Error: fetching or parsing of historical data failed"
    Just incidences -> mapM_ (writeDataPerDay lastUpdate) incidences

fetchAndParse :: Int -> DistrictKey -> IO (Maybe [HistoryFragment])
fetchAndParse nrDays key = parseDistrictHistory key . getResponseBody <$> httpLBS (apiIncidenceHistoryByDistrictRequest nrDays key)

-- reports dont start at the same time and may have gaps. pad with incidence 0 reports
preprocess :: Day -> Day -> [HistoryFragment] -> [HistoryFragment]
preprocess firstReport lastReport fragments = worker fragments $ take (fromIntegral (diffDays lastReport firstReport) + 1) dateGenerator
  where
    dayGenerator = map (flip addDays firstReport) [0 ..]
    dateGenerator = map (\day -> UTCTime {utctDay = day, utctDayTime = 0}) dayGenerator
    fragBuilder aDate = HistoryFragment {date = aDate, weekIncidence = 0}

    worker :: [HistoryFragment] -> [UTCTime] -> [HistoryFragment]
    worker [] [] = []
    worker (_ : _) [] = trace "Warning: preprocessor should not have fragments left but no dates" []
    worker [] (aDate : dates) = fragBuilder aDate : worker [] dates
    worker allFrags@(frag : frags) (aDate : dates)
      | utctDay (date frag) == utctDay aDate = frag : worker frags dates
      | utctDay (date frag) > utctDay aDate = fragBuilder aDate : worker allFrags dates -- missing element
      | otherwise = worker frags dates -- duplicate element, drop one

writeDataPerDay :: Day -> [HistoryFragment] -> IO ()
writeDataPerDay lastUpdate fragments = do
  let theDay = date $ head fragments
      theDayString = pack $ formatTime defaultTimeLocale timeFormat theDay
      --          debugStr = \frag -> show (ApiHistoryData.weekIncidence frag) ++ "__" ++ formatTime defaultTimeLocale timeFormat (ApiHistoryData.date frag)
      values = map (pack . show . weekIncidence) fragments
  when
    (utctDay theDay > lastUpdate)
    (TIO.appendFile historyIncidenceFile $ T.append (T.intercalate ", " (theDayString : values)) "\n")

-- old
saveQueryResultLazy :: Request -> FilePath -> IO ()
saveQueryResultLazy request filename = do
  res <- httpLBS request
  let status = getResponseStatusCode res
  if status == 200
    then do
      print $ "saving response body to " ++ filename
      let body = getResponseBody res
      L.writeFile filename body
    else print $ "request with status code" ++ show status
