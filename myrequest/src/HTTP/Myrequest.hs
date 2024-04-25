{-# LANGUAGE OverloadedStrings #-}
-- | Http request
-- TODO document generation left
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- import HTTP.Myrequest
-- import Data.Aeson
-- import qualified Data.ByteString.Lazy as L

-- main :: IO ()
-- main = do
--     let myData = object 
--                  [ "name" .= ("usernmae"::String)
--                  , "pwd" .= ("password"::String)
--                  ]
--     lStr <- request' $ mpost
--                 $ hAuthorization "xxxxx"
--                 $ withJson myData
--                 $ "https://test-xxx.com/api/activity/user_equity/user_prop_surplus"
--     L.putStr lStr
-- @
-- Socks5 and Socks5h proxy 
-- @
-- main :: IO ()
-- main = do
--   setting <- newManagerWithSocksProxy ("127.0.0.1",7890)
--   resp <- requestWith setting $ mget $ "https://ipinfo.io"
--   print resp
--   putStrLn ""
-- @
-- 'applyBasicProxyAuth'
-- dotJson
-- @
--   setting <- newManagerWithSocksProxy ("127.0.0.1",7890)
--   resp <- requestWith setting $ mget $ "https://ipinfo.io"
--   let v = dotJson $ responseBody $ resp
--   print (v:Maybe Value)
-- @
module HTTP.Myrequest
  ( module HTTP
  , module HTTPS
  , module T
  , mmethod
  , mpost
  , mget
  , mhead
  , mput
  , mdelete
  , setHeader
  , hUtf8json
  , bjson
  , withJson
  , putFormData
  , request
  , request'
  , requestWith
  , newManagerWithSocksProxy
  , setProxy
  , setResponseTimeout
  , dotJson
  , parse
  , setRequestBodyBS
  , setRequestBodyForm
  , s
  , n
  ) where
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.MultipartFormData as HTTP
import Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Method as Method --http-types
import qualified Data.Aeson as A
import qualified Network.HTTP.Types.Header as Header
import Network.HTTP.Types as T
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.ByteString.Lazy
import qualified Data.ByteString as B
import Data.Default.Class
import qualified Network.Connection as NC
import qualified Network.Socket as NS
import qualified Network.HTTP.Client.Internal as HTTPI
import Data.String(fromString)
import Network.HTTP.Types.URI (renderQuery)


mmethod :: Method.Method -> HTTP.Request -> HTTP.Request
mmethod httpMethod req = req {HTTP.method = httpMethod}

{-- 
GET	 
POST	 
HEAD	 
PUT	 
DELETE	 
TRACE	 
CONNECT	 
OPTIONS	 
PATCH
--}
mpost = mmethod Method.methodPost
mget = mmethod Method.methodGet
mhead = mmethod Method.methodHead
mput = mmethod Method.methodPut
mdelete = mmethod Method.methodDelete

s :: String -> String
s = id

n :: Double -> Double
n = id

setHeader :: Header.Header -> HTTP.Request -> HTTP.Request
setHeader oneHeader req = req {HTTP.requestHeaders=oneHeader:HTTP.requestHeaders req}

setProxy :: B.ByteString -> Int -> HTTP.Request -> HTTP.Request
setProxy hostname port req= req {HTTP.proxy = Just (HTTP.Proxy hostname port)}




hUtf8json = setHeader (Header.hContentType,"application/json; charset=utf-8")

hAuthorization auth = setHeader (Header.hAuthorization,auth)

bjson :: A.ToJSON a => a -> HTTP.Request -> HTTP.Request
bjson value req = req {HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode value}

setRequestBodyBS :: B.ByteString -> HTTP.Request -> HTTP.Request
setRequestBodyBS bs req = req {
  HTTP.requestBody = HTTP.RequestBodyBS bs
}

setRequestBodyForm :: [(B.ByteString,Maybe B.ByteString)] -> HTTP.Request -> HTTP.Request
setRequestBodyForm item = setRequestBodyBS  (renderQuery False item) .
  setHeader (Header.hContentType,"application/x-www-form-urlencoded")


withJson :: A.ToJSON a => a -> HTTP.Request -> HTTP.Request
withJson a = bjson a . hUtf8json

-- | form data body
-- > req <- withFormData [partBS "prompt" "A cute baby sea otter wearing a beret", partFileSource "image" "otter.png"] request
putFormData :: MonadIO m => [HTTP.Part] -> HTTP.Request -> m  HTTP.Request
putFormData parts req = HTTP.formDataBody parts req

parse :: String -> HTTP.Request
parse url = setResponseTimeout 0 $ fromString url

setResponseTimeout :: Int -> HTTP.Request -> HTTP.Request
setResponseTimeout micro req | micro == 0 = req {HTTP.responseTimeout = HTTP.responseTimeoutNone}
                             | otherwise = req {HTTP.responseTimeout = HTTP.responseTimeoutMicro micro}

requestWith :: MonadIO m => HTTP.Manager -> HTTP.Request -> m (HTTP.Response ByteString)
requestWith manager req = liftIO $ HTTP.httpLbs req manager

newManagerWithSocksProxy :: (String,NS.PortNumber) -> IO HTTP.Manager
newManagerWithSocksProxy (host,port) = do
  let set = HTTPS.mkManagerSettings def (Just $ NC.SockSettingsSimple host port) 
  let set' = set { HTTP.managerTlsConnection = HTTP.managerTlsConnection set
                 , HTTPI.managerTlsProxyConnection = HTTPI.managerTlsProxyConnection set
                 }
  HTTP.newManager set'

request :: MonadIO m => HTTP.Request -> m (HTTP.Response ByteString)
request req = do
    manager <- liftIO $ HTTPS.newTlsManager
    requestWith manager req

dotJson :: (A.FromJSON v) => ByteString -> Maybe v
dotJson str = A.decode str

request' req = do
  resp <- request req
  pure (HTTP.responseBody resp)

