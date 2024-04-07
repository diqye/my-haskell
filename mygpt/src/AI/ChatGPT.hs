{-# LANGUAGE DeriveGeneric #-}
module AI.ChatGPT where

import qualified HTTP.Myrequest as H
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Reader(ReaderT(ReaderT),ask,runReaderT)
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import Data.Aeson((.=))
import qualified Data.Aeson.Types as A
import Data.String(fromString)
import Data.Text(Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.Class(lift)
import Control.Applicative((<|>))
import Data.IORef
import AI.Datas
import Control.Lens hiding ((.=))
import Data.Aeson.Lens
import Control.Monad
import Mydefault


type GPTError = [A.Value]
type GPT a = ReaderT Config (ExceptT GPTError IO) a

liftExceptT :: (ExceptT GPTError IO) a -> GPT a
liftExceptT = lift

askConfig :: GPT Config
askConfig = ask

runGPT :: GPT a -> Config -> IO (Either [A.Value] a)
runGPT readerBlock r = do
    let exceptBlock = runReaderT readerBlock r
    eitherResult <- runExceptT exceptBlock
    pure eitherResult

gptRequest :: String -> GPT H.Request
gptRequest restPath = do
    config <- askConfig
    let auth = ("Bearer " <>) $ fromString $ config ^. c_apiKey
    pure $ 
        H.setResponseTimeout 0 $
        H.setHeader ("Authorization",  auth) $
        fromString ((config ^. c_baseUrl) <> restPath)

aliRequest :: String -> GPT H.Request
aliRequest restPath = do
    config <- askConfig
    let auth = ("Bearer " <>) $ fromString $ config ^. c_other . _Just . o_ali . _Just . _1 
    let url = config ^. c_other . _Just . o_ali . _Just . _2
    pure $ 
        H.setResponseTimeout 0 $
        H.setHeader ("Authorization",  auth) $
        fromString (url <> restPath)

minimaxRequest :: String -> GPT H.Request
minimaxRequest path = do
    config <- askConfig
    let (auth,url,groupid) = config ^. c_other . _Just . o_minimax . _Just
    pure $
        H.setResponseTimeout 0 $
        H.setHeader ("Content-Type", "application/json") $
        H.setHeader ("Authorization", "Bearer " <> fromString auth) $
        H.setQueryString [("GroupId", Just $ fromString groupid)] $
        fromString $ url <> path


minimaxT2A :: A.Value -> GPT A.Value
minimaxT2A options = do
    req <- minimaxRequest "t2a_pro"
    result <- mypost req options
    let statusCode = result ^? key "base_resp" . key "status_code" . _JSON'
    if statusCode == Just 0 then pure result else do
        liftExceptT $ throwE [result]

transEither :: Monad m => Either String a ->  ExceptT GPTError m a
transEither (Left str) = ExceptT $ pure $ Left [A.toJSON str]
transEither (Right a) = ExceptT $ pure $ Right a




getField key v = A.parseEither id $ A.withObject "Object" parse v where 
    parse obj = obj A..: key
get key v = transEither $ getField key v

putError :: Monad m => A.Value -> ExceptT [A.Value] m a
putError v = do
    err <- get "error" v
    ExceptT $ pure $ Left [err]


fromValue :: (A.FromJSON a,Monad m) => A.Value -> ExceptT [A.Value] m a
fromValue v = case A.fromJSON v of 
    A.Error msg -> ExceptT $ pure $ Left [A.toJSON msg]
    A.Success a -> ExceptT $ pure $ Right a
-- | models
{-
{
    "data":[{"created":1698785189,"id":"dall-e-3","object":"model","owned_by":"system"},{"created":1705953180,"id":"text-embedding-3-large","object":"model","owned_by":"system"},{"created":1686587434,"id":"gpt-3.5-turbo-0613","object":"model","owned_by":"openai"},{"created":1698798177,"id":"dall-e-2","object":"model","owned_by":"system"},{"created":1694122472,"id":"gpt-3.5-turbo-instruct-0914","object":"model","owned_by":"system"},{"created":1677532384,"id":"whisper-1","object":"model","owned_by":"openai-internal"},{"created":1699053533,"id":"tts-1-hd-1106","object":"model","owned_by":"system"},{"created":1699046015,"id":"tts-1-hd","object":"model","owned_by":"system"},{"created":1692634615,"id":"babbage-002","object":"model","owned_by":"system"},{"created":1705948997,"id":"text-embedding-3-small","object":"model","owned_by":"system"},{"created":1692901427,"id":"gpt-3.5-turbo-instruct","object":"model","owned_by":"system"},{"created":1681940951,"id":"tts-1","object":"model","owned_by":"openai-internal"},{"created":1706048358,"id":"gpt-3.5-turbo-0125","object":"model","owned_by":"system"},{"created":1677610602,"id":"gpt-3.5-turbo","object":"model","owned_by":"openai"},{"created":1692634301,"id":"davinci-002","object":"model","owned_by":"system"},{"created":1677649963,"id":"gpt-3.5-turbo-0301","object":"model","owned_by":"openai"},{"created":1699053241,"id":"tts-1-1106","object":"model","owned_by":"system"},{"created":1671217299,"id":"text-embedding-ada-002","object":"model","owned_by":"openai-internal"},{"created":1698959748,"id":"gpt-3.5-turbo-1106","object":"model","owned_by":"system"},{"created":1683758102,"id":"gpt-3.5-turbo-16k","object":"model","owned_by":"openai-internal"},{"created":1685474247,"id":"gpt-3.5-turbo-16k-0613","object":"model","owned_by":"openai"}],
    "object":"list"
}
{
    "error":{"code":"unknown_url","message":"Unknown request URL: GET /v1/ddmodels. Please check the URL for typos, or see the docs at https://platform.openai.com/docs/api-reference/.","param":null,"type":"invalid_request_error"}
}

-}
models :: GPT [Model]
models = do
    req <- gptRequest "models"
    v <- myget req
    liftExceptT $ do
        data' <- get "data" v <|> putError v
        pure data'

 
-- | models/xxxx
retrieveModel :: String ->  GPT Model
retrieveModel modelId = do
    req <- gptRequest ("models/" <> fromString modelId)
    v <- myget req 
    liftExceptT  $ do
        data' <- fromValue v <|> putError v
        pure data'

-- | chat/completions
-- "gpt-3.5-turbo-16k-0613","gpt-3.5-turbo-0125","gpt-3.5-turbo","gpt-3.5-turbo-0613","gpt-3.5-turbo-0301","gpt-3.5-turbo-instruct-0914","gpt-3.5-turbo-instruct","tts-1-1106","text-embedding-ada-002","gpt-3.5-turbo-1106","gpt-3.5-turbo-16k
-- https://platform.openai.com/docs/api-reference/chat/create
chatWith :: Text -> [Message] -> A.Value -> GPT ChatObject
chatWith model messages options = do
    req <- gptRequest "chat/completions"
    v <- mypost req newOptions
    liftExceptT $ do
        data' <- fromValue  v <|> putError v
        pure data' 
    where json = A.object [ "model" A..= model
            , "messages" A..= messages
            , "stream" A..= False
            ]
          newOptions = options <<>> json

-- | {"id":"chatcmpl-8zbuFTbty9eu3SuJb19gGSUxPqswD","object":"chat.completion.chunk","created":1709694411,"model":"gpt-35-turbo","choices":[{"finish_reason":null,"index":0,"delta":{"content":" today"},"content_filter_results":{"hate":{"filtered":false,"severity":"safe"},"self_harm":{"filtered":false,"severity":"safe"},"sexual":{"filtered":false,"severity":"safe"},"violence":{"filtered":false,"severity":"safe"}}}]}
type Callback a = IO (Either B.ByteString ChunkModel) -> IO a
chatStreamWith :: Text -> [Message] -> A.Value -> Callback a -> GPT a
chatStreamWith model messages options callback = do
    req' <- gptRequest "chat/completions"
    let req = 
            H.withJson (json & _Object %~ (<> (options ^. _Object))) $ 
            H.mpost req'
    config <- askConfig
    liftIO $ do
        manager <-  config ^. c_managerAction
        H.withResponse req manager $ \ resp -> do
            ref <- newIORef []
            lastRef <- newIORef Nothing
            f resp ref lastRef
    where json = A.object [ "model" A..= model
              , "messages" A..= messages
              , "stream" A..= True
              ]
          f resp ref lastRef = callback r where
              r = do
                list <- readIORef ref
                lastBs <- readIORef lastRef
                if null list then do
                    let body = H.responseBody resp
                    bs <- H.brRead body
                    let lines =
                          filter (not .B.null) $ 
                          B.lines bs
                    let (nLs,objs) = go lines lastBs 0
                    writeIORef ref objs
                    -- putStrLn "======"
                    -- forM_ lines B.putStrLn
                    -- print nLs
                    -- putStrLn "====end===="                    
                    writeIORef lastRef nLs
                    if B.null bs then 
                        pure $ case lastBs of 
                            Nothing -> Left ""
                            Just bs -> trans bs
                    else r 
                else do
                    let (x:xs) = list
                    writeIORef ref xs
                    pure x
          go [] Nothing _ = (Nothing,[])
          go [] (Just bs) _ = (Just bs,[])
          go [x] (Just bs) 0 = go' (bs <> x) $ trans (bs <> x)
          go [x] _ _ = go' x $ trans x
          go (x:xs) (Just bs) 0 = (lastItem,trans (bs <>x):xs') where
              (lastItem,xs') = go xs Nothing 1
          go (x:xs) bsm n = (lastItem,trans x:xs') where
              (lastItem,xs') = go xs bsm (n+1)
          go' _ r@(Right _) = (Nothing,[r])
          go' bs (Left _) = (Just bs,[])
          
          trans bs | B.null bs = Left ""
                   | bs == "data: [DONE]" = Left ""
                   | otherwise = decodeEither bs
          decodeEither :: B.ByteString -> Either B.ByteString ChunkModel
          decodeEither bs = case A.eitherDecodeStrict (B.drop 6 bs) of
              Right a -> Right a
              Left _ -> Left bs


defOption = A.object []

chat :: Text -> [Message] -> GPT ChatObject
chat model messages = chatWith model messages defOption

chatStream :: Text -> [Message] -> Callback a -> GPT a
chatStream model message callback = chatStreamWith model message defOption callback

imageGenerate :: ImageBody -> GPT A.Value
imageGenerate body = do
    req <- gptRequest "images/generations"
    v <- mypost req body
    let maybeData = v ^? key "data"
    maybe (throw' [v]) (const $ pure v) maybeData
    where throw' v = liftExceptT $ throwE v

speechOption :: Text -> A.Value
speechOption input = A.object
    [ "model" A..= "tts-1-hd"
    , "input" A..= input
    , "voice" A..= "shimmer"
    ]
speech :: A.Value -> GPT L.ByteString
speech option = do
    req <- gptRequest "audio/speech"
    config <- askConfig
    liftIO $ do
        manager <- config ^. c_managerAction
        resp <- H.requestWith manager $
            H.withJson option $ 
            H.mpost req
        pure $ H.responseBody resp

qwenTurbo = "qwen-turbo" :: Text
qwenVlPlus = "qwen-vl-plus" :: Text
-- | 不同的模型有不同的参数格式
-- qwen-turbo
-- qwen-vl-plus
aliChatWith :: Text -> [Message] -> A.Value -> GPT A.Value
aliChatWith model messages options = do
    req <- aliRequest url
    mypost req newOptions
    where newOptions = A.object [
                "model" .= model ,
                "input" .= A.object [
                    "messages" .= messages 
                ],
                "parameters" .=   options <<>> A.object [
                    "result_format" .= H.s "message"
                ]
            ]
          url | model == qwenVlPlus = "services/aigc/multimodal-generation/generation"
              | otherwise = "services/aigc/text-generation/generation"
mypost :: (A.ToJSON a, A.FromJSON b) => H.Request -> a -> GPT b
mypost req mydata = do
    config <- askConfig
    value <- liftIO $ do
        manager <- config ^. c_managerAction
        let req' = H.withJson mydata $ H.mpost req
        resp <- H.requestWith manager req'
        config ^. c_logHandle $ (req',resp)
        let lazyStr = H.responseBody resp
        -- L.putStrLn lazyStr
        let either = A.eitherDecode lazyStr
        pure either
    liftExceptT $ transEither value

myget :: (A.FromJSON b) => H.Request -> GPT b
myget req = do
    config <- askConfig
    value <- liftIO $ do
        manager <- config ^. c_managerAction
        let req' = H.hUtf8json $ H.mget req
        resp <- H.requestWith manager req'
        config ^. c_logHandle $ (req',resp)
        let lazyStr = H.responseBody resp
        let either = A.eitherDecode lazyStr
        pure either
    liftExceptT $ transEither value
createAliImageOptions :: Maybe Integer -> Text -> A.Value
createAliImageOptions seed prompt = A.object [
        "model" .= "wanx-v1" ,
        "input" .= A.object [
            "prompt" .= prompt ,
            "negative_prompt" .= ""

        ],
        "parameters" .= A.object [
            "style" .= "<flat illustration>",
            "seed" .= seed,
            "n" .= 1,
            "size" .= "720*1280"
        ]
    ]

-- | 文生图
-- services/aigc/text2image/image-synthesis
aliImageGenerate :: A.Value -> GPT A.Value
aliImageGenerate options = do
    baseReq <- aliRequest "services/aigc/text2image/image-synthesis"
    let req = H.setHeader ("X-DashScope-Async","enable") baseReq
    v <- mypost req options
    let taskStatus = v ^? key "output" . key "task_status"
    maybe (throw' [v]) (const $ pure v) taskStatus
    where throw' v = liftExceptT $ throwE v

aliTask :: String -> GPT A.Value
aliTask taskId = do
    req <- aliRequest $ "tasks/" ++ taskId
    myget req

-- | services/aigc/background-generation/generation/
aliBackgroundGenerate :: A.Value -> GPT A.Value
aliBackgroundGenerate options = do
    baseReq <- aliRequest "services/aigc/background-generation/generation"
    let req = H.setHeader ("X-DashScope-Async","enable") baseReq
    mypost req options

aliSemantic :: A.Value -> GPT A.Value
aliSemantic options = do
    baseReq <- aliRequest "services/aigc/wordart/semantic"
    let req = H.setHeader ("X-DashScope-Async","enable") baseReq
    mypost req options

aliTexture :: A.Value -> GPT A.Value 
aliTexture options = do
    baseReq <- aliRequest "services/aigc/wordart/texture"
    let req = H.setHeader ("X-DashScope-Async","enable") baseReq
    mypost req options
