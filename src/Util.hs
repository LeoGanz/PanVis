module Util where

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

bsToL :: BC.ByteString -> L.ByteString
bsToL = L.fromChunks . return
