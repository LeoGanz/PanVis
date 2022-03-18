module Util where

import Control.Exception.Base (IOException, catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Text as T
import Data.Text.Encoding as T

textToBS :: Text -> BC.ByteString
textToBS = T.encodeUtf8

sToBS :: String -> BC.ByteString
sToBS = textToBS . T.pack

intToBS :: Int -> BC.ByteString
intToBS = sToBS . show

intToText :: Int -> Text
intToText = T.pack . show

bsToL :: BC.ByteString -> L.ByteString
bsToL = L.fromChunks . return

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

-- adapted from https://stackoverflow.com/a/51729478
readFileOrElse :: FilePath -> String -> IO String
readFileOrElse filePath def =
  readFile filePath
    `catch` \e -> const (return def) (e :: IOException)
