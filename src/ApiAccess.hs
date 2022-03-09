{-# LANGUAGE OverloadedStrings #-}

module ApiAccess where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple

-- ArcGis apis
arcgisHost :: BC.ByteString
arcgisHost = "services7.arcgis.com"

arcgisServicePathPrefix :: BC.ByteString
arcgisServicePathPrefix = "/mOBPykOjAyBO2ZKk/arcgis/rest/services"

arcgisHistoryEndpoint :: BC.ByteString
arcgisHistoryEndpoint = "/rki_history_v"

arcgisStatusEndpoint :: BC.ByteString
arcgisStatusEndpoint = "/rki_service_status_v"

arcgisQueryPrefix :: BC.ByteString
arcgisQueryPrefix = "/FeatureServer/0/query"

arcgisQueryUrl :: BC.ByteString -> [(BC.ByteString, BC.ByteString)] -> BC.ByteString
arcgisQueryUrl endpoint params = arcgisHost <> arcgisServicePathPrefix <> endpoint <> arcgisQueryPrefix <> buildQueryParams params

arcgisDefaultParams :: [(BC.ByteString, BC.ByteString)]
arcgisDefaultParams =
  [ ("f", "json"),
    ("where", "1=1"),
    ("outFields", "*")
  ]

arcgisHistoryRequest :: Request
arcgisHistoryRequest = buildRequest "GET" arcgisHost (arcgisServicePathPrefix <> arcgisHistoryEndpoint <> arcgisQueryPrefix) arcgisDefaultParams

arcgisStatusRequest :: Request
arcgisStatusRequest = buildRequest "GET" arcgisHost (arcgisServicePathPrefix <> arcgisStatusEndpoint <> arcgisQueryPrefix) arcgisDefaultParams

-- General api access methods

buildQueryParams :: [(BC.ByteString, BC.ByteString)] -> BC.ByteString
buildQueryParams = foldl (\acc (key, val) -> acc <> key <> "=" <> val <> "&") "?"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> [(BC.ByteString, BC.ByteString)] -> Request
buildRequest method host path params =
  let queryParams = buildQueryParams params
   in setRequestMethod method $
        setRequestHost host $
          setRequestPath (path <> queryParams) $
            setRequestPort 443 $
              setRequestSecure
                True
                defaultRequest

saveQueryResult :: Request -> String -> IO ()
saveQueryResult request filename = do
  res <- httpLBS request
  let status = getResponseStatusCode res
  if status == 200
    then do
      print $ "saving response body to " ++ filename
      let body = getResponseBody res
      L.writeFile filename body
    else print $ "request with status code" ++ show status
