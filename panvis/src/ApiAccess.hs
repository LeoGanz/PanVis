{-# LANGUAGE OverloadedStrings #-}

module ApiAccess where
-- ideas for rest api access are taken from https://livebook.manning.com/book/get-programming-with-haskell/chapter-39/28

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

arcgisHost :: BC.ByteString
arcgisHost = "services7.arcgis.com"

servicePathPrefix :: BC.ByteString
servicePathPrefix = "/mOBPykOjAyBO2ZKk/arcgis/rest/services"

historyEndpoint :: BC.ByteString
historyEndpoint = "/rki_history_v"

statusEndpoint :: BC.ByteString
statusEndpoint = "/rki_service_status_v"

queryPrefix :: BC.ByteString
queryPrefix = "/FeatureServer/0/query"

buildQueryUrl endpoint = arcgisHost <> servicePathPrefix <> endpoint <> queryPrefix

defaultParams = [("f", "json"), 
                 ("where", "1=1"),
                 ("outFields", "*")
                ]
           
buildQueryParams :: [(BC.ByteString, BC.ByteString)] -> BC.ByteString           
buildQueryParams params = foldl (\acc (key, val) -> acc <> key <> "=" <> val <> "&") "?" params

buildRequest method host path params = 
    let queryParams = buildQueryParams params in 
        setRequestMethod method
        $ setRequestHost host
        $ setRequestPath (path <> queryParams)
        $ setRequestPort 443
        $ setRequestSecure True
        $ defaultRequest
    
historyRequest = buildRequest "GET" arcgisHost (servicePathPrefix <> historyEndpoint <> queryPrefix) defaultParams
statusRequest = buildRequest "GET" arcgisHost (servicePathPrefix <> statusEndpoint <> queryPrefix) defaultParams

saveQueryResult request filename = do
    res <- httpLBS request
    let status = getResponseStatusCode res
    if status == 200
        then do
            print $ "saving response body to " ++ filename
            let body = getResponseBody res
            L.writeFile filename body
        else print $ "request with status code" ++ show status
        
testSaveStatus = saveQueryResult statusRequest "service-status.json"