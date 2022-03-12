module DataRetrieval.AesonUtil where

import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

-- all code below is adapted from https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html

(.->) :: FromJSON a => Parser Object -> Text -> Parser a
(.->) parser key = do
  obj <- parser
  obj .: key

-- list instance for FromJSON already exists
newtype JSONHashList a = HashList [a]
  deriving (Show)

instance FromJSON a => FromJSON (JSONHashList a) where
  parseJSON = withObject "JSONHashList" $ \obj ->
    let kvs = HM.toList obj
        values = map snd kvs
        parsed = mapM parseJSON values
     in HashList <$> parsed
