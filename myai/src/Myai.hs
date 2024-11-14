{-# LANGUAGE QuasiQuotes #-}
module Myai where

import           Myai.Data.Config          (Config(Config,_ollama, _gpt, _azure), MonadAIReader, Error, MonadAI, manager, MonadAIError, createError)                                                                                                        -- 
import qualified Myai.Data.GPT             as G                                                                                                                                                                                                             -- 
import qualified Myai.Data.Azure           as A                                                                                                                                                                                                             -- 
import           HTTP.Myrequest            (BodyReader, Request, Response(responseBody, responseStatus), brRead, withResponse, mpost, parse, setHeader, withJson, setResponseTimeout, HttpException(HttpExceptionRequest), requestWith, statusIsSuccessful) -- 
import qualified Myai.Data.Ollama          as O                                                                                                                                                                                                             -- 
import           Control.Monad.Reader      (ReaderT, runReaderT, MonadReader(ask), asks)                                                                                                                                                                    -- mtl-2.3.1
import           Control.Monad.Except      (ExceptT, runExceptT, liftEither, throwError, MonadError(throwError))                                                                                                                                            -- mtl-2.3.1
import           Control.Monad.Cont        (ContT, MonadCont(..))                                                                                                                                                                                           -- mtl-2.3.1
import           Control.Lens              ((^.), (&), ix, (%~), (^?))                                                                                                                                                                                      -- lens-5.2.3
import           Data.Text                 (Text)                                                                                                                                                                                                           -- text-2.0.2
import           Control.Monad.IO.Class    (MonadIO(..))                                                                                                                                                                                                    -- base-4.18.2.1
import           Data.Maybe                (maybeToList, isJust)                                                                                                                                                                                            -- base-4.18.2.1
import           Control.Monad             (when, unless)                                                                                                                                                                                                   -- base-4.18.2.1
import           Data.Monoid               (First(First))                                                                                                                                                                                                   -- base-4.18.2.1
import           Control.Exception         (catch, Exception(displayException), SomeException(SomeException))                                                                                                                                               -- base-4.18.2.1
import           Data.Aeson                (Value(Null), decodeStrict, ToJSON(toJSON), FromJSON, eitherDecode)                                                                                                                                              -- aeson-2.1.2.1
import           Data.Aeson.Lens           (key)                                                                                                                                                                                                            -- lens-aeson-1.2.3
import           Text.QuasiText            (embed)                                                                                                                                                                                                          -- QuasiText-0.1.2.6
import           Data.ByteString           (ByteString)                                                                                                                                                                                                     -- bytestring-0.11.5.3
import qualified Data.ByteString           as B                                                                                                                                                                                                             -- bytestring-0.11.5.3
import qualified Data.ByteString.Char8     as B                                                                                                                                                                                                             -- bytestring-0.11.5.3
import           Control.Monad.Trans.Class (MonadTrans(lift))                                                                                                                                                                                               -- transformers-0.6.1.0
import           Control.Monad.Trans.Cont  (evalContT)                                                                                                                                                                                                      -- transformers-0.6.1.0
import           Data.String.Conversions   (cs)                                                                                                                                                                                                             -- string-conversions-0.4.0.1

runAIT ::  Config -> ReaderT Config (ExceptT Error m) a ->  m (Either Error a)
runAIT config mAI = runExceptT $ runReaderT mAI config


useConfig :: MonadAIReader m => m Config
useConfig = ask


useGPTRequest :: MonadAI m => String -> m Request
useGPTRequest restPath = do
    (Config {_gpt=gpt}) <- useConfig
    let key = gpt ^. G.key
    when (null key) $ throwError $ createError "No GPT config"
    pure $
        setHeader ("Authorization", cs $ "Bearer " <>  key) $
        parse $ gpt ^. G.baseUrl <> restPath

useOllamaRequest :: MonadAI m => String -> m Request
useOllamaRequest restPath = do
    (Config {_ollama=o}) <- useConfig
    let baseURL = o ^. O.baseURL
    when (null baseURL) $ throwError $ createError "No Ollama config"
    pure $ parse $ baseURL <> restPath

useAzureRequest :: MonadAI m => String -> m Request
useAzureRequest deploymentId = do
    (Config {_azure=A.Azure { A._endpoint=endpoint,A._key=key}}) <- useConfig
    when (null key) $ throwError $ createError "No Azure config"
    pure $
        setHeader ("api-key", cs key) $
        setResponseTimeout 0 $
        parse $
        cs [embed|$endpoint/openai/deployments/$deploymentId/chat/completions?api-version=2024-06-01|]

type ResponseStream = Response BodyReader
type MonadStream a = ContT a (ReaderT ResponseStream  (ExceptT Error IO)) a
useStream :: (MonadAI m, ToJSON d, MonadIO m)
    => d -> Request -> MonadStream a -> m a
useStream mydata request mb = do
    config <- useConfig
    let mgrIO = config ^. manager
    eitherV <- liftIO $ do
        mgr <-  mgrIO
        let req = withJson mydata $ mpost request
        let myerror :: HttpException -> IO (Either Error a)
            myerror (HttpExceptionRequest _ content) = pure $ Left $ createError $ show content
            myerror e = pure $ Left $ createError $ show e
        let myerror2 :: SomeException -> IO (Either Error a)
            myerror2 = pure . Left . createError . ("diqye" <>) . show
        withResponse req mgr (\x -> (runExceptT  . flip runReaderT x . evalContT) mb)
            `catch` myerror
            `catch` myerror2
    liftEither  eitherV

-- | 无限 recur
createLabel :: MonadCont m => a -> m (a -> m b,a)
createLabel a = callCC $ \ exit -> do
    let go b = exit (go,b)
    pure (go,a)

fixData :: ByteString -> ByteString
fixData bs
    | B.take 5 bs == "data:" = B.drop 5 bs
    | otherwise = bs


-- | 解析 sse数据 :data xxxx 一次chunk中可能是多行，最后一行可能是不全的JSON数据，
-- 多次读取下,要分析和拼接不全的数据。
recurValues :: (MonadReader ResponseStream m, MonadAIError m,MonadIO m) => a -> ContT r m (a -> ContT r m (), [Value],a)
recurValues a = do
    br <- lift $ asks responseBody
    (recur, (lastBs,a)) <- createLabel ("",a)
    bs' <- liftIO $ brRead br
    let lines = fmap fixData (B.lines bs')  & ix 0 %~ (lastBs <>)
    let maybeValues = fmap decodeStrict lines
    let values = do {
        maybeV <- maybeValues;
        maybeToList maybeV
    }
    let isAllPass = all isJust maybeValues
    let lastBs = if isAllPass then "" else last lines
    let recur' a = do {
        if B.null bs' then
            pure ()
        else
            recur (lastBs,a);
            pure ()
    }
    -- when (not (B.null bs') && null values) $ do
    --     lift $ throwError $ First $ Just $ toJSON  (cs bs' :: Text)
    when (null values) $ recur' a
    unless (null values) $ do
        let v = head values
        let a = v ^? key "error"
        when (isJust a) $ lift $ liftEither $ Left $ createError a
    pure (recur',values,a)
-- | 一次提取一个Value
recurValue ::  (MonadReader ResponseStream m, MonadAIError m,MonadIO m) => a -> ContT r m (a -> ContT r m (), Value,a)
recurValue a = do
    (recur,values',a') <- recurValues a
    (recur1,(xs',a2)) <- createLabel (values',a')
    if null xs' then do
        let fn _ = pure ()
        pure (fn,Null,a2)
    else do
        let (x:xs) = xs'
        let fn a | null xs = do
                recur a
                pure ()
            fn a = do
                _ <- recur1 (xs,a)
                pure ()
        pure (fn,x,a2)


mapLeft :: (a -> c) -> Either a b -> Either c b 
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

usePost :: (ToJSON d, MonadAI m, FromJSON a, MonadIO m) => d -> Request -> m a
usePost mydata request = do
    config <- useConfig
    eitherV <- liftIO $ do
        mgr <-  config ^. manager
        resp <- requestWith mgr $ 
            withJson mydata $
            mpost request
        let isSuccess = statusIsSuccessful $ responseStatus resp
        let body = responseBody resp
        if isSuccess then 
            pure $ eitherDecode body
        else pure $ Left  (show resp)
    liftEither $ mapLeft createError eitherV

{- 


eitherTransString :: Either String b -> Either AIError b
eitherTransString = mapLeft $ First . Just . A.toJSON

useGET :: (MonadAI m, A.FromJSON a, MonadIO m) => H.Request -> m a
useGET request = do
    mgrIO <- use'conf manager
    eitherV <- liftIO $ do
        mgr <-  mgrIO
        resp <- H.requestWith mgr $ H.mget request
        let isSuccess = H.statusIsSuccessful $ H.responseStatus $ resp
        let body = H.responseBody resp
        if isSuccess then 
            pure $ A.eitherDecode body
        else pure $ Left (show resp)
    liftEither $ eitherTransString $ eitherV


-- useMicrosoftSpeech :: (MonadAI m,MonadIO m) => ByteString -> m L.ByteString
-- useMicrosoftSpeech  xml = do
--     (key,url) <- use'conf microsoft
--     mgrIO <- use'conf manager
--     mgr <- liftIO mgrIO
--     -- liftIO $ B.putStrLn xml
--     resp <- H.requestWith mgr $
--         H.setRequestBodyBS xml $
--         H.setHeader ("Ocp-Apim-Subscription-Key",cs $ key) $
--         H.setHeader ("Content-Type","application/ssml+xml") $
--         H.setHeader ("X-Microsoft-OutputFormat","audio-16khz-32kbitrate-mono-mp3") $
--         H.setHeader ("User-Agent","Haskell") $
--         H.mpost $
--         H.parse url
--     let status@(H.Status code msg) = H.responseStatus resp
--     when (not $ H.statusIsSuccessful $ status) $ throwError $ First $ Just $ A.toJSON $ (code,(cs msg :: String))
--     pure $ H.responseBody resp


chatStream :: (MonadAI m, A.ToJSON d, MonadIO m) 
    => d ->  (MonadStream a) -> m a
chatStream myData mb = do
    req <- useGPTRequest  "chat/completions"
    useStream myData req mb

-}