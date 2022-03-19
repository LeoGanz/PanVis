{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}

module History
  ( History (..),
    Header (..),
    Body (..),
    DistrictInfo (..),
    Snapshot (..),
    parseHistory,
    parseBodyPlain,
    parseHistoryPlain,
    removeBlanks,
    appendBody,
    lastDay,
    dropHeader
  )
where

import Data.Char (isSpace)
import Data.Data
import Data.Time.Calendar (Day, fromGregorian)
import Language.Haskell.TH.Syntax hiding (Body)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number

data History
  = History Header Body
  | AntiHistory String -- history from string var
  | ExtendedHistory String Body -- history supplied by history var via $hist: , body parsed normally
  | AntiExtendedHistory String String -- history supplied by history var via $hist: , body parsed from string var
  deriving (Show, Typeable, Data)

newtype Header = Header [DistrictInfo]
  deriving (Show, Typeable, Data)

newtype Body = Body [Snapshot]
  deriving (Show, Typeable, Data, Lift)

data DistrictInfo = DistrictInfo {ags :: String, name :: String, stateAbbreviation :: String, population :: Int}
  deriving (Show, Typeable, Data)

data Snapshot = Snapshot Day [Double]
  deriving (Show, Typeable, Data, Lift)

instance Lift Day where
  lift = dataToExpQ (const Nothing)
  liftTyped = unsafeTExpCoerce . lift

appendBody :: History -> Body -> History
appendBody (History h (Body b)) (Body b') = History h $ Body (b ++ b')
appendBody _ _ = error "invalid call. appendBody only supports default histories"

lastDay :: History -> Day
lastDay (History _ (Body snaps)) = extractDay $ last snaps
  where
    extractDay (Snapshot day _) = day
lastDay _ = error "invalid call. lastDay only supports default histories"

dropHeader :: [String] -> [String]
dropHeader = tail . dropWhile (/= "") -- header and body are separated by empty line

-- for how to use parsec http://book.realworldhaskell.org/read/using-parsec.html was used among other resources
followArrow = string "=>"

history =
  try (History <$> header <*> body)
    <|> try antiExtendedHistory
    <|> try extendedHistory
    <|> antiHistory

textCell = many (noneOf ",\\\n\r")

header = do
  -- with some effort one could make the order of these fields irrelevant
  agss <- stringInfoFields "ags"
  names <- stringInfoFields "name"
  stateAbbreviations <- stringInfoFields "stateAbbreviation"
  populations <- integerInfoFields "population"
  let districts = map (\i -> DistrictInfo {ags = agss !! i, name = names !! i, stateAbbreviation = stateAbbreviations !! i, population = populations !! i}) [0 .. (length names - 1)]
  possiblyEscapedEol -- blank line
  return $ Header districts

stringInfoFields infoType = string infoType *> followArrow *> sepBy textCell (char ',') <* possiblyEscapedEol

integerInfoFields infoType = string infoType *> followArrow *> sepBy int (char ',') <* possiblyEscapedEol

body = Body <$> endBy dataRow possiblyEscapedEol

dataRow = do
  day <- date
  followArrow
  values <- sepBy cell (char ',')
  return $ Snapshot day values

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

antiHistory = AntiHistory <$> stringVar

extendedHistory = ExtendedHistory <$> histVar <* possiblyEscapedEol <*> body

antiExtendedHistory = AntiExtendedHistory <$> histVar <* optional possiblyEscapedEol <*> stringVar

histVar = string "$hist:" *> ident <?> "history variable"

stringVar = char '$' *> ident <?> "string variable"

-- code for small, large, idchar & ident adapted from guide https://www.schoolofhaskell.com/user/marcin/quasiquotation-101 and https://wiki.haskell.org/Quasiquotation

small = lower <|> char '_'

large = upper

idchar = small <|> large <|> digit <|> char '\'' <?> "char of identifier"

ident = (:) <$> small <*> many idchar <?> "identifier for variable"

parseHistoryPlain :: String -> Either ParseError History
parseHistoryPlain = parse history "(unknown)" . removeBlanks

parseBodyPlain :: String -> Either ParseError Body
parseBodyPlain = parse body "(unknown)" . removeBlanks

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
      e <- history
      eof
      return e

removeBlanks :: String -> String
removeBlanks xs = [a | a <- xs, a == '\n' || not (isSpace a)]
