{-# LANGUAGE DeriveGeneric,TemplateHaskell #-}

module AI.Datas where
    
import qualified HTTP.Myrequest as H
import Control.Lens hiding((.=))
import Data.Text(Text)
import Data.Aeson
import Data.List(intercalate)
import Data.Default.Class
import GHC.Generics
import Control.Applicative(empty)

createMyOptions :: Int -> Options
createMyOptions n = defaultOptions {
    fieldLabelModifier = drop n,
    sumEncoding = UntaggedValue
}
trancateOptions :: Options
trancateOptions = defaultOptions {
    fieldLabelModifier = tail . dropWhile (/= '\''),
    sumEncoding = UntaggedValue,
    omitNothingFields = True
}
data OtherConfig = OtherConfig {
    -- (key,url)
    _o_ali :: Maybe (String,String)
} deriving Show

makeLenses ''OtherConfig

data Config = Config { 
    _c_apiKey :: String ,
    _c_baseUrl :: String ,
    _c_managerAction :: IO H.Manager ,
    _c_other :: Maybe OtherConfig
}

makeLenses ''Config

instance Show Config where
    show a = intercalate "\n" outputs where
        apiKey = a ^. c_apiKey
        baseUrl = a ^. c_baseUrl
        other = a ^. c_other
        outputs = [
                "apiKey=" <> apiKey,
                "baseUrl=" <> baseUrl,
                "ohter.ali=" <> show other
            ]
instance Default Config where
    def = Config {
        _c_apiKey = "",
        _c_baseUrl = "https://api.openai.com/v1/",
        _c_managerAction = H.newTlsManager ,
        _c_other = Nothing
    }

data Model = Model { 
    _m_id :: Text ,
    _m_created :: Integer ,
    _m_object :: Text ,
    _m_owned_by :: Text 
} deriving (Show,Read,Generic)

instance ToJSON Model where
    toJSON = genericToJSON $ createMyOptions 3
instance FromJSON Model where
    parseJSON = genericParseJSON $ createMyOptions 3

makeLenses ''Model

data MessageRoleUser = MessageRoleUser deriving (Show,Read)
data MessageRoleAssistant = MessageRoleAssistant deriving (Show,Read)
data MessageRoleSystem = MessageRoleSystem deriving (Show,Read)
data MessageRoleTool = MessageRoleTool deriving (Show,Read)

instance ToJSON MessageRoleUser where
    toJSON _ = String "user"
instance FromJSON MessageRoleUser where
    parseJSON (String "user") = pure MessageRoleUser
    parseJSON _ = empty
instance ToJSON MessageRoleAssistant where
    toJSON _ = String "assistant"
instance FromJSON MessageRoleAssistant where
    parseJSON (String "assistant") = pure MessageRoleAssistant
    parseJSON _ = empty
instance ToJSON MessageRoleSystem where
    toJSON _ = String "system"
instance FromJSON MessageRoleSystem where
    parseJSON (String "system") = pure MessageRoleSystem
    parseJSON _ = empty
instance ToJSON MessageRoleTool where
    toJSON _ = String "tool"
instance FromJSON MessageRoleTool where
    parseJSON (String "tool") = pure MessageRoleTool
    parseJSON _ = empty



data Message = MsgUser {
    _user'role :: MessageRoleUser ,
    _user'name :: Maybe Text ,
    _user'content :: Text
} | MsgAssistant {
    _msg'role :: MessageRoleAssistant ,
    _msg'name :: Maybe Text ,
    _msg'content :: Text ,
    _msg'tool_calls :: Maybe [Value]
} | MsgSystem {
    _sys'role :: MessageRoleSystem ,
    _sys'name :: Maybe Text ,
    _sys'content :: Text
} | MsgTool {
    _tool'role :: MessageRoleTool ,
    _tool'content :: Text ,
    _tool'call_id :: Text
} | MsgCustom Value deriving (Show,Read,Generic)

makeLenses ''Message

user :: Text -> Message
user content = MsgUser {
    _user'role = MessageRoleUser ,
    _user'name = Nothing ,
    _user'content = content
}
data MContent = MImage Text | MText Text deriving Show
userImage :: [MContent] -> Message
userImage xs = MsgCustom $ object
    [ "role" .= H.s "user"
    , "content" .= map trans xs
    ] where
    trans (MImage text) = object
        [ "type" .= H.s "text"
        , "text" .= text
        ]
    trans (MText text) = object
        [ "type" .= H.s "image_url"
        , "image_url" .= object
              [ "url" .= text
              ]
        ]

vlMsg :: Text -> [MContent] -> Message
vlMsg role xs = MsgCustom $ object [
        "role" .= role ,
        "content" .= map trans xs
    ] where
    trans (MImage text) = object [
            "image" .= text
        ]
    trans (MText text) = object [ 
            "text" .= text
        ]


userWith :: Text -> Text -> Message
userWith name content = MsgUser {
    _user'role = MessageRoleUser ,
    _user'name = Just name ,
    _user'content = content
}
assistant :: Text -> Message
assistant content = MsgAssistant {
    _msg'role = MessageRoleAssistant ,
    _msg'name = Nothing ,
    _msg'content = content ,
    _msg'tool_calls = Nothing
}
system :: Text -> Message 
system content = MsgSystem {
    _sys'role = MessageRoleSystem ,
    _sys'name = Nothing ,
    _sys'content = content
}
instance ToJSON Message where
    toJSON = genericToJSON trancateOptions

instance FromJSON Message where
    parseJSON = genericParseJSON trancateOptions

data ChatChoice = ChatChoice {
    _choice_index :: Int ,
    _choice_message :: Message
} deriving (Show,Read,Generic)

instance ToJSON ChatChoice where
    toJSON = genericToJSON $ createMyOptions 8

instance FromJSON ChatChoice where
    parseJSON = genericParseJSON $ createMyOptions 8

makeLenses ''ChatChoice


data ChatUsage = ChatUsage {
    _usage_prompt_tokens :: Int ,
    _usage_completion_tokens :: Int ,
    _usage_total_tokens :: Int
} deriving  (Show,Read,Generic)

instance ToJSON ChatUsage where
    toJSON = genericToJSON $ createMyOptions 7

instance FromJSON ChatUsage where
    parseJSON = genericParseJSON $ createMyOptions 7

makeLenses ''ChatUsage


data ChatObject = ChatObject { 
    _chat_id :: Text ,
    _chat_object :: Text ,
    _chat_created :: Integer ,
    _chat_model :: Text ,
    _chat_system_fingerprint :: Maybe Text ,
    _chat_choices :: [ChatChoice] ,
    _chat_usage :: ChatUsage
} deriving (Show,Read,Generic)

instance ToJSON ChatObject where
    toJSON = genericToJSON $ createMyOptions 6

instance FromJSON ChatObject where
    parseJSON = genericParseJSON $ createMyOptions 6

makeLenses ''ChatObject
{--
{
    "id":"chatcmpl-8zbuFTbty9eu3SuJb19gGSUxPqswD",
    "object":"chat.completion.chunk",
    "created":1709694411,
    "model":"gpt-35-turbo",
    "choices":[{
        "finish_reason":null,
        "index":0,
        "delta":{"content":" today"},
        "content_filter_results":{
            "hate":{
                "filtered":false,
                "severity":"safe"
            },
        "self_harm":{
            "filtered":false,
            "severity":"safe"
        },
        "sexual":{
            "filtered":false,
            "severity":"safe"
        },
        "violence":{"filtered":false,"severity":"safe"}
        }
    }]
}
--}
-- | "delta":{}
data ChoiceDelta = ChoiceContent {
    _chunk_c_content :: Text
} | ChoiceRole {
    _chunk_c_role :: Text
} | ChoiceEmpty {
    _chunk_c_empty :: Maybe ()
} deriving (Show,Read,Generic)

instance ToJSON ChoiceDelta where
    toJSON = genericToJSON $ createMyOptions 9

instance FromJSON ChoiceDelta where
    parseJSON = genericParseJSON $ createMyOptions 9

makeLenses ''ChoiceDelta

data ChunkChoice = ChunkChoice {
    _chunk_c_finish_reason :: Maybe Text,
    _chunk_c_index :: Int,
    _chunk_c_delta :: ChoiceDelta,
    _chunk_c_content_filter_results :: Maybe Value,
    _chunk_c_self_harm :: Maybe Value,
    _chunk_c_sexual :: Maybe Value,
    _chunk_c_violence :: Maybe Value
} deriving (Show,Read,Generic)
instance ToJSON ChunkChoice where
    toJSON = genericToJSON $ createMyOptions 9

instance FromJSON ChunkChoice where
    parseJSON = genericParseJSON $ createMyOptions 9
    
makeLenses ''ChunkChoice 
data ChunkModel = ChunkModel {
    _chunk_id :: !Text,
    _chunk_object :: !Text,
    _chunk_created :: !Integer,
    _chunk_model :: !Text,
    _chunk_system_fingerprint :: !(Maybe Text) ,
    _chunk_choices :: ![ChunkChoice]
} deriving (Show,Read,Generic)

instance ToJSON ChunkModel where
    toJSON = genericToJSON $ createMyOptions 7

instance FromJSON ChunkModel where
    parseJSON = genericParseJSON $ createMyOptions 7

makeLenses ''ChunkModel

instance Default Text where
    def = ""
-- ｜ https://platform.openai.com/docs/api-reference/images/create
data ImageBody = ImageBody {
    _img'prompt :: !Text,
    _img'model :: Maybe Text,
    _img'n :: Maybe Int,
    _img'quality :: Maybe Text,
    _img'response_format :: Maybe Text,
    _img'size :: Maybe Text,
    _img'style :: Maybe Text,
    _img'user :: Maybe Text
} deriving (Show,Generic)

instance Default ImageBody

makeLenses ''ImageBody

instance ToJSON ImageBody where
    toJSON = genericToJSON trancateOptions

instance FromJSON ImageBody where
    parseJSON = genericParseJSON trancateOptions