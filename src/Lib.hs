{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( someFunc,
    historyFromFile,
    historyFromDefaultFile,
    fetchAndSaveData
  )
where

import History
import HistoryQuote
import DataRetrieval.ApiDataManager

someFunc :: IO ()
someFunc = print testQQ

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

stringToHistory :: String -> History
stringToHistory s = [history|$s|]

--directHistoryFromFile = [history_f|incidence.history|] -- works only for small files
historyFromFile :: FilePath -> IO History
historyFromFile file = readFile file >>= \s -> return $ stringToHistory s

historyFromDefaultFile :: IO History
historyFromDefaultFile = historyFromFile "incidence.history"

fetchAndSaveData :: IO ()
fetchAndSaveData = updateHistoryIncidenceFile
