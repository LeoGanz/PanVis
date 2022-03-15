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
testQQ = [history|asd,das\n2020-12-02=>123,444.9,89\n12-12-12=>12,13,14.5\n|]
