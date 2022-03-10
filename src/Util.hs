module Util where

import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Text (Text)
import Data.Text.Encoding as T

textToBS :: Text -> BC.ByteString
textToBS = T.encodeUtf8

intToBS :: Bin.Binary a => a -> BC.ByteString
intToBS i = toStrict $ Bin.encode i
