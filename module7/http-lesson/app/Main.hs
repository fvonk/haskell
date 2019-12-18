module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status

myToken :: BC.ByteString
myToken = "ZdgeUVXGiROYwlXKbKiscCukAuZjBgwl"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatus response
  if (statusCode status) == 200
    then do
      putStrLn "Results were saved"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else
      putStrLn $ "Error occured: " ++ ((BC.unpack . statusMessage) status)
      -- print $ statusMessage status



-- Request

buildRequest :: BC.ByteString -> BC.ByteString
             -> BC.ByteString -> BC.ByteString
             -> Request
buildRequest token host method path =
    setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure False
  -- True
  $ setRequestPort 80
  -- 443
  $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath
