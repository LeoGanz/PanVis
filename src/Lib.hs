{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( someFunc,
  )
where

import History
import HistoryQuote

someFunc :: IO ()
someFunc = print testQQ

testQQ :: History
testQQ = [history|
ags               => 09162, otherAgs
name              => d1, d2
stateAbbreviation => BY, BY
population        => 1234, 23455

2020-12-02        => 123, 444.9
2012-12-12        => 12, 13.23
|]
