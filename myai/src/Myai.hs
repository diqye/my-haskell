{-# LANGUAGE TemplateHaskell #-}
module Myai where

import Data.Default.Class
import Mydefault
import Control.Lens
import Data.Aeson.Lens
import Control.Monad.Except
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.String.Conversions(cs)
import qualified Data.Aeson as A
import Control.Monad(guard,forM_)
import Control.Applicative((<|>),Alternative)
import Control.Monad.Reader
import qualified HTTP.Myrequest as H
import Data.Monoid
import Control.Monad.IO.Class
import System.Environment 
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Cont hiding (callCC)

(<--) :: (A.KeyValue kv,A.ToJSON v) => A.Key -> v -> kv
(<--) = (A..=)

type AIError = First A.Value

-- instance Semigroup AIError where
--     a <> _ = a
-- instance Monoid AIError where
--     mempty = AIError $ A.toJSON "mempty"

data AIConfigGPTStyle = AIConfigGPTStyle {
    _apiKey :: String ,
    _baseURL :: String 
}
data AIConfig = AIConfig {
    _gptStyle :: AIConfigGPTStyle ,
    _manager :: IO H.Manager ,
    _ali :: (String,String) ,
    _minimax :: (String,String,String)
}

makeLenses ''AIConfigGPTStyle

instance Default AIConfig where
    def = AIConfig {
        _gptStyle = AIConfigGPTStyle {
            _apiKey = def ,
            _baseURL = def
        } ,
        _manager = H.newTlsManager ,
        _ali = def ,
        _minimax = def
    }

makeLenses ''AIConfig

type MonadAIReader m = MonadReader AIConfig m
type MonadAIError m = MonadError AIError m
type MonadAI m = (Alternative m, MonadAIReader m, MonadAIError m)

runAIT ::  ReaderT AIConfig (ExceptT AIError m) a -> AIConfig -> m (Either AIError a)
runAIT mAI config = runExceptT $ runReaderT mAI config

-- | Like `runAIT` , but config generated from enviroment variables `GPT_KEY` and `GPT_BASE_URL`
-- example in fish.sh:
-- set -x GPT_KEY s-xxxxx
-- set -x GPT_BASE_URL https://oneapi.xxx.com/v1/
runEnvAIT :: (MonadIO m) => ReaderT AIConfig (ExceptT AIError m) a -> m (Either AIError a)
runEnvAIT mAI = do
    (key,url) <- liftIO $ do
        key <- getEnv "GPT_KEY"
        url <- getEnv "GPT_BASE_URL"
        pure (key,url)
    runAIT mAI $ def {
        _gptStyle = AIConfigGPTStyle {
            _apiKey = key ,
            _baseURL = url
        }
    }

useConfig :: MonadAIReader m => m AIConfig
useConfig = ask

use' :: MonadAIReader m => Getting a AIConfig a ->  m a
use' getting = do 
    config <- useConfig
    pure $ config ^. getting

useGPTRequest :: MonadAIReader m => String -> m H.Request 
useGPTRequest restPath = do
    style <- use' gptStyle
    pure $
        H.setHeader ("Authorization", cs $ _apiKey style) $
        H.parse $ _baseURL style <> restPath

useAliRequest :: MonadAIReader m => String -> m H.Request
useAliRequest restPath = do
    (url,key) <- use' ali
    pure $
        H.setHeader ("Authorization", cs key) $
        H.parse $ url <> restPath


mapLeft :: (a -> c) -> Either a b -> Either c b 
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

eitherTransString :: Either String b -> Either AIError b
eitherTransString a = mapLeft  (First . Just . A.toJSON) a


useGET :: (MonadAI m, A.FromJSON a, MonadIO m) => H.Request -> m a
useGET request = do
    mgrIO <- use' manager
    eitherV <- liftIO $ do
        mgr <-  mgrIO
        resp <- H.requestWith mgr $ H.mget request
        let isSuccess = H.statusIsSuccessful $ H.responseStatus $ resp
        let body = H.responseBody resp
        if isSuccess then 
            pure $ A.eitherDecode body
        else pure $ Left (show resp)
    liftEither $ eitherTransString $ eitherV

usePOST :: (A.ToJSON d, MonadAI m, A.FromJSON a, MonadIO m) => d -> H.Request -> m a
usePOST mydata request = do
    mgrIO <- use' manager
    eitherV <- liftIO $ do
        mgr <-  mgrIO
        resp <- H.requestWith mgr $ 
            H.withJson mydata $
            H.mpost request
        let isSuccess = H.statusIsSuccessful $ H.responseStatus $ resp
        let body = H.responseBody resp
        if isSuccess then 
            pure $ A.eitherDecode body
        else pure $ Left (show resp)
    liftEither $ eitherTransString $ eitherV

type ResponseStream = H.Response H.BodyReader
type MonadStream a = ContT a (ReaderT ResponseStream  (ExceptT AIError IO)) a
useStream :: (MonadAI m, A.ToJSON d, MonadIO m) 
    => d -> H.Request -> MonadStream a -> m a
useStream mydata request mb = do
    mgrIO <- use' manager
    eitherV <- liftIO $ do
        mgr <-  mgrIO
        let req = H.withJson (A.toJSON mydata <<>> A.object ["stream" <-- True]) $ H.mpost request
        H.withResponse req mgr $ \x -> (runExceptT  . flip runReaderT x . evalContT) mb
    liftEither $ eitherV

-- | 无限 recur
createLabel :: MonadCont m => a -> m (a -> m b,a)
createLabel a = callCC $ \ exit -> do
    let go b = exit (go,b)
    pure (go,a)

-- | 解析 sse数据 :data xxxx 一次chunk中可能是多行，最后一行可能是不全的JSON数据，
-- 多次读取下,要分析和拼接不全的数据。
recurValues :: (MonadReader ResponseStream m, MonadAIError m,MonadIO m) => a -> ContT r m (a -> ContT r m (), [A.Value],a)
recurValues a = do
    br <- lift $ fmap H.responseBody ask
    (recur, (lastBs,a)) <- createLabel ("",a)
    bs' <- liftIO $ H.brRead br
    -- liftIO $ B.putStrLn bs'
    when (not (B.null bs') && B.take 5 bs' /= "data:") $ do
        let maybeV = A.decodeStrict bs'
        lift $ throwError $ maybe (First $ Just $ A.String $ cs $ bs') (First . Just) maybeV

    let lines = fmap (B.drop 6) (B.lines bs') & ix 0 %~ (lastBs <>)
    let maybeValues = fmap A.decodeStrict lines
    let maybeToBool = maybe False (const True)
    let values = do {
        maybeV <- maybeValues;
        maybe [] (:[]) maybeV
    }
    let isAllPass = all maybeToBool maybeValues
    let lastBs = if isAllPass then "" else last lines
    let recur' a = do {
        if B.null bs' then
            pure ()
        else 
            recur (lastBs,a);
            pure ()
    }
    pure (recur',values,a)

chatStream :: (MonadAI m, A.ToJSON d, MonadIO m) 
    => d ->  (MonadStream a) -> m a
chatStream myData mb = do
    req <- useGPTRequest  "chat/completions"
    useStream myData req mb

