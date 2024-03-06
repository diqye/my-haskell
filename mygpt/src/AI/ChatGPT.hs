{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module AI.ChatGPT where

import qualified HTTP.Myrequest as H
import Control.Monad.IO.Class(liftIO,MonadIO)
import Control.Monad.Trans.Reader(ReaderT(ReaderT),ask,runReaderT)
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.String(fromString)
import Data.Text(Text)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.Class(lift)
import Control.Applicative((<|>))
import GHC.Generics

data GPTConfig = GPTConfig
    { api_key :: String
    , base_url :: String
    , manager_action :: IO H.Manager
    } 
instance Show GPTConfig where
    show a = api_key a <> " " <> base_url a

type GPT a = ReaderT GPTConfig (ExceptT [A.Value] IO) a

liftExceptT = lift

runGPT :: GPT a -> GPTConfig -> IO (Either [A.Value] a)
runGPT readerBlock r = do
    let exceptBlock = runReaderT readerBlock r
    eitherResult <- runExceptT exceptBlock
    pure eitherResult

gptRequest :: String -> GPT H.Request
gptRequest restPath = do
    config <- ask
    pure $ 
        H.setResponseTimeout 0 $
        H.setHeader ("Authorization", "Bearer " <> fromString (api_key config)) $
        fromString (base_url config <> restPath)

data Model = Model { _id :: Text
    , created :: Integer
    , object :: Text
    , owned_by :: Text
    } deriving (Show,Read)
instance A.ToJSON Model where
    toJSON model = A.object [ "id" A..= _id model
        , "created" A..= created model
        , "object" A..= object model
        , "owned_by" A..= owned_by model
        ]

instance A.FromJSON Model where
    parseJSON = A.withObject "Model" $ \v -> do
        id' <- v A..: "id"
        object' <- v A..: "object" 
        created' <- v A..: "created"
        owned_by' <- v A..: "owned_by"
        pure $ Model { _id = id'
            , created = created'
            , object = object'
            , owned_by = owned_by'
            }





transEither :: Monad m => Either String a ->  ExceptT [A.Value] m a
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
    config <- ask
    value <- liftIO $ do
        manager <- manager_action config
        resp <- H.requestWith manager $
            H.hUtf8json $
            H.mget req
        let either = A.eitherDecode $ H.responseBody resp
        pure either
    liftExceptT $ do
        v <- transEither value
        data' <- get "data" v <|> putError v
        pure data'

 
-- | models/xxxx
retrieveModel :: String ->  GPT Model
retrieveModel modelId = do
    req <- gptRequest ("models/" <> fromString modelId)
    config <- ask
    value <- liftIO $ do
        manager <- manager_action config
        resp <- H.requestWith manager $
            H.hUtf8json $
            H.mget req
        pure $ A.eitherDecode $ H.responseBody resp
    liftExceptT  $ do
        v <- transEither value
        data' <- fromValue v <|> putError v
        pure data'


data Role = User | Assistant | System | Custom Text deriving (Show,Read)
roleText :: Role -> Text
roleText User = "user"
roleText Assistant = "assistant"
roleText System = "system"
roleText (Custom a) = a

textRole :: Text -> Role
textRole "user" = User
textRole "assistant" = Assistant
textRole "system" = System
textRole a = Custom a

data Message = Message (Role,Text) deriving (Show,Read)
instance A.ToJSON Message where
   toJSON (Message (role,content)) = A.object 
        [ "role" A..= roleText role
        , "content" A..= content
        ]
instance A.FromJSON Message where
    parseJSON = A.withObject "Message" $ \v -> do
        role <- v A..: "role"
        content <- v A..: "content" 
        pure $ Message (textRole role,content)

myOptions n = A.defaultOptions { A.fieldLabelModifier = drop n, A.sumEncoding=A.UntaggedValue}
data ChatObject = ChatObject { chat_id :: Text
    , chat_object :: Text
    , chat_created :: Integer
    , chat_model :: Text
    , chat_system_fingerprint :: Maybe Text
    , chat_choices :: [ChatChoice]
    , chat_usage :: ChatUsage
    } deriving (Show,Read,Generic)

instance A.ToJSON ChatObject where
    toJSON = A.genericToJSON (myOptions 5)

instance A.FromJSON ChatObject where
    parseJSON = A.genericParseJSON (myOptions 5)

data ChatChoice = ChatChoice { choice_index :: Int
    , choice_message :: Message
    } deriving (Show,Read,Generic)

instance A.ToJSON ChatChoice where
    toJSON = A.genericToJSON (myOptions 7)

instance A.FromJSON ChatChoice where
    parseJSON = A.genericParseJSON (myOptions 7)

data ChatUsage = ChatUsage { usage_prompt_tokens :: Int
    , usage_completion_tokens :: Int
    , usage_total_tokens :: Int
    } deriving  (Show,Read,Generic)

instance A.ToJSON ChatUsage where
    toJSON = A.genericToJSON (myOptions 6)

instance A.FromJSON ChatUsage where
    parseJSON = A.genericParseJSON (myOptions 6)

unValue :: A.Value -> A.Object
unValue (A.Object obj) = obj

-- | chat/completions
-- "gpt-3.5-turbo-16k-0613","gpt-3.5-turbo-0125","gpt-3.5-turbo","gpt-3.5-turbo-0613","gpt-3.5-turbo-0301","gpt-3.5-turbo-instruct-0914","gpt-3.5-turbo-instruct","tts-1-1106","text-embedding-ada-002","gpt-3.5-turbo-1106","gpt-3.5-turbo-16k
-- https://platform.openai.com/docs/api-reference/chat/create
chatWith :: Text -> [Message] -> A.Value -> GPT ChatObject
chatWith model messages options = do
    req <- gptRequest "chat/completions"
    config <- ask
    value <- liftIO $ do
        manager <- manager_action config
        resp <- H.requestWith manager $
            H.withJson (A.Object (unValue json <> unValue options)) $ 
            H.mpost req
        let either = A.eitherDecode $ H.responseBody resp
        pure either
    liftExceptT $ do
        v <- transEither value
        data' <- fromValue  v <|> putError v
        pure data' 
    where json = A.object [ "model" A..= model
            , "messages" A..= messages
            ]

-- | {"id":"chatcmpl-8zbuFTbty9eu3SuJb19gGSUxPqswD","object":"chat.completion.chunk","created":1709694411,"model":"gpt-35-turbo","choices":[{"finish_reason":null,"index":0,"delta":{"content":" today"},"content_filter_results":{"hate":{"filtered":false,"severity":"safe"},"self_harm":{"filtered":false,"severity":"safe"},"sexual":{"filtered":false,"severity":"safe"},"violence":{"filtered":false,"severity":"safe"}}}]}
type Callback a = IO (Either String A.Value) -> IO a
chatStreamWith :: Text -> [Message] -> A.Value -> Callback a -> GPT a
chatStreamWith model messages options callback = do
    req' <- gptRequest "chat/completions"
    let req = 
            H.withJson (A.Object (unValue json <> unValue options)) $ 
            H.mpost req'
    config <- ask
    liftIO $ do
        manager <- manager_action config
        H.withResponse req manager f
    where json = A.object [ "model" A..= model
              , "messages" A..= messages
              , "stream" A..= True
              ]
          f resp = callback r where
              r = do
                  let body = H.responseBody resp
                  bs <- H.brRead body
                  putStrLn "bs ======"
                  B.putStrLn bs
                  putStrLn "bs end======"
                  let lines =
                          filter (not .B.null) $ 
                          B.lines bs
                --   let objs = map trans lines
                --   pure objs
                  undefined
          trans bs | B.null bs = Left ""
                   | bs == "data: [DONE]" = Left ""
                   | otherwise = A.eitherDecodeStrict $ B.drop 6 bs
          printRequest req = do
              putStrLn "print request"
              print req
              putStrLn "end"