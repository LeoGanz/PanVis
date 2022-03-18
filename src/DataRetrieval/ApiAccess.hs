{-# LANGUAGE OverloadedStrings #-}

module DataRetrieval.ApiAccess
  ( buildRequest,
    arcgisHost,
    arcgisHistoryRequest,
    arcgisStatusRequest,
    arcgisQueryUrl,
    apiHost,
    apiIncidenceHistoryByDistrictRequest,
    apiDistrictsRequest,
    apiHistoryIncidenceRequest,
    daysInOverviewRequest
  )
where

import qualified Data.ByteString.Char8 as BC
import DataRetrieval.ApiDistrict
import Network.HTTP.Simple
import Util

-- General api access methods

buildQueryParams :: [(BC.ByteString, BC.ByteString)] -> BC.ByteString
buildQueryParams [] = ""
buildQueryParams params = foldl (\acc (key, val) -> acc <> key <> "=" <> val <> "&") "?" params

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

-- ArcGis apis - not used anymore
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

-- using  https://api.corona-zahlen.org -- by Marlon Lückert

daysInOverviewRequest :: Int
daysInOverviewRequest = 40

apiHost :: BC.ByteString
apiHost = "api.corona-zahlen.org"

apiDistrictsPrefix :: BC.ByteString
apiDistrictsPrefix = "/districts"

apiHistoryIncidence :: BC.ByteString
apiHistoryIncidence = "/history/incidence"

apiHistoryIncidenceRequest :: Request
apiHistoryIncidenceRequest = buildRequest "GET" apiHost (apiDistrictsPrefix <> apiHistoryIncidence <> "/" <> intToBS daysInOverviewRequest) []

apiIncidenceHistoryByDistrictRequest :: Int -> ApiDistrictKey -> Request
apiIncidenceHistoryByDistrictRequest days ags = buildRequest "GET" apiHost (apiDistrictsPrefix <> "/" <> textToBS ags <> apiHistoryIncidence <> "/" <> intToBS days) []

apiDistrictsRequest :: Request
apiDistrictsRequest = buildRequest "GET" apiHost apiDistrictsPrefix []
