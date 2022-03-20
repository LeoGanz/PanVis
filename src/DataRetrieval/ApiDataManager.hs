{-# LANGUAGE OverloadedStrings #-}

module DataRetrieval.ApiDataManager
  ( getDistrictsList,
    historyIncidenceFile,
    updateHistoryIncidenceFile,
    saveQueryResultLazy,
    updateFileSuffix,
    preloadFileSuffix,
    parseDateFromLine
  )
where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import Data.List (transpose)
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.IO.Utf8 as Utf8
import Data.Time
import DataRetrieval.ApiAccess
import DataRetrieval.ApiDistrict (ApiDistrict, ApiDistrictKey, ags, name, parseDistrictList, population, stateAbbreviation)
import DataRetrieval.ApiHistoryData
import DataRetrieval.HistoryUpdate (parseHistoryUpdate, processHistoryUpdates)
import Debug.Trace (trace)
import Network.HTTP.Simple
import Safe (lastMay)
import Util

getDistrictsList :: IO (Maybe [ApiDistrict])
getDistrictsList = parseDistrictList . getResponseBody <$> httpLBS apiDistrictsRequest

historyIncidenceFile :: FilePath
historyIncidenceFile = "incidence.history"

updateFileSuffix :: String
updateFileSuffix = "-update"

preloadFileSuffix :: String
preloadFileSuffix = "-preload"

historyIncidenceUpdateFile :: FilePath
historyIncidenceUpdateFile = historyIncidenceFile ++ updateFileSuffix

historyIncidencePreloadFile :: FilePath
historyIncidencePreloadFile = historyIncidenceFile ++ preloadFileSuffix

timeFormat :: String
timeFormat = "%Y-%m-%d"

followArrow :: T.Text
followArrow = " => "

updateHistoryIncidenceFile :: IO ()
updateHistoryIncidenceFile = do
  distsMay <- getDistrictsList
  case distsMay of
    Nothing -> print "Error: Could not load districts data from Api"
    Just dists -> do
      let agss = map ags dists
          names = map name dists
          states = map stateAbbreviation dists
          populations = map population dists
      TIO.appendFile historyIncidenceFile ""
      TIO.appendFile historyIncidencePreloadFile ""
      TIO.appendFile historyIncidenceUpdateFile ""
      contents <- readFile historyIncidenceFile
      contentsPreload <- readFile historyIncidencePreloadFile
      contentsUpdate <- readFile historyIncidenceUpdateFile
      when
        (null (lines contents))
        ( Utf8.writeFile -- with TIO: problems with utf8 encoding of Umlaute in district names
            historyIncidenceFile
            ( -- header line
              T.concat
                [ "ags",
                  followArrow,
                  T.intercalate ", " agss,
                  "\n",
                  "name",
                  followArrow,
                  T.intercalate ", " names,
                  "\n",
                  "stateAbbreviation",
                  followArrow,
                  T.intercalate ", " states,
                  "\n",
                  "population",
                  followArrow,
                  T.intercalate ", " (map intToText populations),
                  "\n",
                  "\n" -- empty line
                ]
            )
        )
      let lastUpdate = determineLastUpdate [contentsPreload, contents, contentsUpdate]
      case trace ("Last updated: " ++ show lastUpdate ++ " fetching new data ...") lastUpdate of
        Nothing -> doUpdate agss dayZero -- fetch data from the beginning
        Just lastUpdate -> do
          now <- utctDay <$> getCurrentTime
          when
            (addDays 1 lastUpdate < now)
            ( if addDays (toInteger daysInOverviewRequest) lastUpdate > now
                then doUpdatePartial lastUpdate
                else doUpdate agss lastUpdate
            )

determineLastUpdate :: [String] -> Maybe Day
determineLastUpdate = maximum . map (lastUpdate . lines)
  where
    lastUpdate contentLines = case lastMay contentLines of
      Nothing -> Nothing
      Just "" -> lastUpdate $ init contentLines
      Just s -> parseDateFromLine s

parseDateFromLine :: String -> Maybe Day
parseDateFromLine line = parseTimeM True defaultTimeLocale timeFormat $ takeWhile (not . flip elem (" =>," :: String)) line :: Maybe Day

doUpdatePartial :: Day -> IO ()
doUpdatePartial lastUpdate = do
  histUpdates <- parseHistoryUpdate . getResponseBody <$> httpLBS apiHistoryIncidenceRequest
  let updateRows = processHistoryUpdates <$> histUpdates
  case updateRows of
    Nothing -> print "Error: fetching or parsing of update data failed"
    Just rows -> mapM_ (writeDataPerDay (historyIncidenceFile ++ updateFileSuffix) lastUpdate) rows

doUpdate :: [ApiDistrictKey] -> Day -> IO ()
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
    Just incidences -> mapM_ (writeDataPerDay historyIncidenceFile lastUpdate) incidences

fetchAndParse :: Int -> ApiDistrictKey -> IO (Maybe [HistoryFragment])
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

writeDataPerDay :: FilePath -> Day -> [HistoryFragment] -> IO ()
writeDataPerDay file lastUpdate fragments = do
  let theDay = date $ head fragments
      theDayText = pack $ formatTime defaultTimeLocale timeFormat theDay
      --          debugStr = \frag -> show (ApiHistoryData.weekIncidence frag) ++ "__" ++ formatTime defaultTimeLocale timeFormat (ApiHistoryData.date frag)
      values = map (pack . show . weekIncidence) fragments
  when
    (utctDay theDay > lastUpdate)
    ( TIO.appendFile file $
        T.concat
          [ theDayText,
            followArrow,
            T.intercalate ", " values,
            "\n"
          ]
    )

-- old, intended for arcgis api
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
