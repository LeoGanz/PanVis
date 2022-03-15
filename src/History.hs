{-# LANGUAGE DeriveDataTypeable #-}

module History
  ( History (..),
    HistoryHeader (..),
    HistoryRow (..),
    parseHistory,
    parseHistoryPlain,
    removeBlanks,
  )
where

import Data.Char (isSpace)
import Data.Data
import Data.Time.Calendar (Day, fromGregorian)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

data History = History HistoryHeader [HistoryRow]
  deriving (Show, Typeable, Data)

newtype HistoryHeader = HistoryHeader [String]
  deriving (Show, Typeable, Data)

data HistoryRow = HistoryRow Day [Double]
  deriving (Show, Typeable, Data)

-- for how to use parsec http://book.realworldhaskell.org/read/using-parsec.html was used among other resources

historyP = History <$> header <*> endBy dataRow possiblyEscapedEol

header = HistoryHeader <$> sepBy headerCell (char ',') <* possiblyEscapedEol

headerCell = many (noneOf ",\\\\\n\r")

dataRow = do
  day <- date
  string "=>"
  values <- sepBy cell (char ',')
  return $ HistoryRow day values

date :: CharParser st Day
date = do
  year <- int
  char '-'
  month <- int
  char '-'
  day <- int
  return $ fromGregorian year month day

cell :: CharParser st Double
cell = try floating <|> (fromIntegral <$> int) -- support values without dot

possiblyEscapedEol =
  try escapedEol
    <|> eol
    <?> "possibly escaped eol"

escapedEol =
  try (string "\\n\r")
    <|> try (string "\\n\\r")
    <|> try (string "\\r\n")
    <|> try (string "\\r\\n")
    <|> string "\\n"
    <|> string "\\r"
    <?> "escsaped end of line"

eol =
  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseHistoryPlain :: String -> Either ParseError History
parseHistoryPlain = parse historyP "(unknown)" . removeBlanks

-- adapted from wiki QQ Expr example https://wiki.haskell.org/Quasiquotation
parseHistory :: Monad m => (String, Int, Int) -> String -> m History
parseHistory (file, line, col) s =
  let stripped = removeBlanks s
   in case runParser p () "" stripped of
        Left err -> error $ show err
        Right e -> return e
  where
    p = do
      pos <- getPosition
      setPosition $
        flip setSourceName file $
          flip setSourceLine line $
            flip setSourceColumn col $
              pos
      spaces
      e <- historyP
      eof
      return e

removeBlanks :: String -> String
removeBlanks xs = [a | a <- xs, a == '\n' || not (isSpace a)]
