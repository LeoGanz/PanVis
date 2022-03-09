module TestApiAccess where

import ApiAccess

testSaveArcgisStatus :: IO ()
testSaveArcgisStatus = saveQueryResult arcgisStatusRequest "arcgis-service-status.json"
