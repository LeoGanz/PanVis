{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( historyFromFile,
    historyFromDefaultFile,
    fetchAndSaveData,
    appendHistoryWithRawBodyData,
    stringToHistory,
  )
where

import DataRetrieval.ApiDataManager
import History
import HistoryQuote

appendHistoryWithRawBodyData :: History -> String -> History
appendHistoryWithRawBodyData x s = [history|$hist:x $s|]

stringToHistory :: String -> History
stringToHistory s = [history|$s|]

--directHistoryFromFile = [history_f|incidence.history|] -- works only for small files
historyFromFile :: FilePath -> IO History
historyFromFile file = readFile file >>= \s -> return $ stringToHistory s

historyFromDefaultFile :: IO History
historyFromDefaultFile = historyFromFile "incidence.history"

fetchAndSaveData :: IO ()
fetchAndSaveData = updateHistoryIncidenceFile

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
