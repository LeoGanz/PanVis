{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( combinedHistoryFromFile,
    combinedHistoryFromDefaultFile,
    historyFromFile,
    fetchAndSaveData,
    appendHistoryWithRawBodyData,
    stringToHistory,
    testQQ,
    testExtQQ,
    firstDateOfPandemic,
    dataInFrontendFormatPreloaded,
    dataInFrontendFormatUpdates
  )
where

import Control.Lens
import qualified Data.ByteString.Char8 as BC
import Data.Time.Calendar (Day, fromGregorian)
import DataRetrieval.ApiDataManager
import DataStructure
import History
import HistoryQuote
import Util

appendHistoryWithRawBodyData :: History -> String -> History
appendHistoryWithRawBodyData x s = [history|$hist:x $s|]

stringToHistory :: String -> History
stringToHistory s = [history|$s|]

historyFromFile :: FilePath -> IO History
historyFromFile file = readFile file >>= \content -> return $ stringToHistory content

--directHistoryFromFile = [history_f|incidence.history|] -- works only for small files
combinedHistoryFromFile :: FilePath -> IO History
combinedHistoryFromFile file =
  readFileMay (file ++ preloadFileSuffix)
    >>= \preloadStringMay ->
      readFile file
        >>= \histString ->
          readFileOrElse (file ++ updateFileSuffix) ""
            >>= \extensionString -> do
              let withoutPreload@(History _ body) = appendHistoryWithRawBodyData (stringToHistory histString) extensionString
              case preloadStringMay of
                Nothing -> return withoutPreload
                Just "" -> return withoutPreload
                Just preloadString -> return $ appendBody (stringToHistory preloadString) body

combinedHistoryFromDefaultFile :: IO History
combinedHistoryFromDefaultFile = combinedHistoryFromFile historyIncidenceFile

fetchAndSaveData :: IO ()
fetchAndSaveData = updateHistoryIncidenceFile

-- could be read from files, but that causes all kinds of problems with read / write synchronisation and lazy IO. Let's call it a constant
firstDateOfPandemic :: Day
firstDateOfPandemic = fromGregorian 2020 01 08

-- utility functions for front end:

-- can be called right away
dataInFrontendFormatPreloaded :: Day -> IO (Maybe (String, [(BC.ByteString, Double)]))
dataInFrontendFormatPreloaded = dataInFrontendFormat (historyFromFile historyIncidencePreloadFile)

-- only call after fetchAndSaveData is actually executed
dataInFrontendFormatUpdates :: Day -> IO (Maybe (String, [(BC.ByteString, Double)]))
dataInFrontendFormatUpdates = dataInFrontendFormat combinedHistoryFromDefaultFile

dataInFrontendFormat :: IO History -> Day -> IO (Maybe (String, [(BC.ByteString, Double)]))
dataInFrontendFormat hist day = do
  countryTupMay <- flip fromHistory day <$> hist
  let countryMay = countryTupMay ^? _Just . _2
  case countryMay of
    Nothing -> return Nothing
    Just country -> do
      let districtsList = country ^. states ^.. traverse ^. traverse . districts ^.. traverse
          agsList = over mapped textToBS $ districtsList ^.. traverse . districtKey
          incidenceList = districtsList ^.. traverse . weekIncidence
          resultList = zip agsList incidenceList
      return $ Just (formatGermanDate day, resultList)

-- for testing:

testQQ :: History
testQQ =
  [history|
ags               => 09162, otherAgs
name              => d1, d2
stateAbbreviation => BY, BY
population        => 1234, 23455

2020-12-02        => 123, 444.9
2012-12-12        => 12, 13.23
|]

testExtQQ :: History -> History
testExtQQ x = [history|$hist:x\n2088-12-02        => 3453, 44554.9\n2012-12-18        => 1552, 13.2355\n|]
