{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( historyFromFile,
    historyFromDefaultFile,
    fetchAndSaveData,
    appendHistoryWithRawBodyData,
    stringToHistory,
    countryFromDefaultFile,
    testQQ,
    testExtQQ,
    firstDateOfPandemic,
    dataInFrontendFormat,
  )
where

import Control.Lens
import qualified Data.ByteString.Char8 as BC
import Data.Time.Calendar (Day)
import DataRetrieval.ApiDataManager
import DataStructure
import History
import HistoryQuote
import Util

appendHistoryWithRawBodyData :: History -> String -> History
appendHistoryWithRawBodyData x s = [history|$hist:x $s|]

stringToHistory :: String -> History
stringToHistory s = [history|$s|]

--directHistoryFromFile = [history_f|incidence.history|] -- works only for small files
historyFromFile :: FilePath -> IO History
historyFromFile file =
  readFile file
    >>= \histString ->
      readFileOrElse (file ++ updateFileSuffix) ""
        >>= \extensionString -> return $ appendHistoryWithRawBodyData (stringToHistory histString) extensionString

historyFromDefaultFile :: IO History
historyFromDefaultFile = historyFromFile historyIncidenceFile

countryFromDefaultFile :: Day -> IO (Maybe (Day, Country))
countryFromDefaultFile day = flip fromHistory day <$> historyFromDefaultFile

fetchAndSaveData :: IO ()
fetchAndSaveData = updateHistoryIncidenceFile

firstDateOfPandemic :: IO (Maybe Day)
firstDateOfPandemic = readFileMay historyIncidenceFile >>= \content -> return $ (parseDateFromLine . head . dropHeader . lines) =<< content

-- utility function for front end
dataInFrontendFormat :: Day -> IO (Maybe (String, [(BC.ByteString, Double)]))
dataInFrontendFormat day = do
  countryTupMay <- countryFromDefaultFile day
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
