{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import Data.String (fromString)
import HTTP.Myrequest
import System.Environment (getEnv)

test :: String
test = intercalate "\n"
  [ "Enjoy the following conversation:"
  , "Sweet and Sour Fish: Do you eat sugar?"
  , "Three-Strong Soup: I want to eat what's in your mouth."
  , "Sweet and Sour Fish: Get lost."
  , "Sweet and Sour Fish: Hehe."
  , "Three-Strong Soup: I won't stick out my tongue then."
  , "Sweet and Sour Fish: Get lost."
  , "Sweet and Sour Fish: Hehe."
  , "Three-Strong Soup: Fine, I'll stick out my tongue."
  , "Sweet and Sour Fish: Get lost."
  , "Sweet and Sour Fish: Hehe."
  , "Three-Strong Soup: Do I really need to stick out my tongue or not?"
  ]

tts :: IO ()
tts = do
  key <- getEnv "API_KEY_DOUYU"
  let myData = object
        [ "model" .= s "tts-1-hd"
        , "input" .=
          s
            "从前，有一个小村庄，村庄里住着一只叫做小灰兔的可爱小兔子。小灰兔是村庄上最快乐的孩子，他总是笑嘻嘻地跑来跑去，喜欢和其他动物一起玩耍。一天，小灰兔遇到了一只迷路了的小松鼠，小松鼠哭着说找不到家了。小灰兔心生怜悯，便主动带着小松鼠找寻她的家。经过了一番冒险和困难，他们终于找到了小松鼠的家，小松鼠的爸爸妈妈十分感激小灰兔的帮助。从此，小灰兔和小松鼠成了最好的朋友，他们经常在森林里玩耍，分享彼此的快乐和悲伤。\n\n有一天，一只凶猛的狼出现在了村庄附近，它想要吃掉小灰兔和小松鼠。小灰兔和小松鼠吓得躲在角落里颤抖，但突然，所有村庄的动物们团结起来，一起对抗凶狼。他们联合起来，发挥各自的特长，最终成功地把狼赶跑了。\n\n在那之后，小灰兔和小松鼠明白到，只有团结合作才能战胜困难。他们明白了友谊的真正含义，从此过着快乐和和谐的生活。村庄里的每个动物都变得更加团结、友爱，一起努力打造一个更美好的家园。\n\n这个故事告诉我们，团结合作和友谊是非常重要的。当我们面对困难时，只要伸出援手，互相帮助，就一定能够战胜困难，创造美好的未来。希望你也能像小灰兔和小松鼠一样，珍惜友谊，勇敢面对困难，做一个勇敢、善良的小朋友。安安稳稳地入睡吧，晚安！愿你做一个勇敢善良的小孩，梦里的世界充满温暖和爱意。"
        , "voice" .= s "nova"
        , "speed" .= n 1
        ]
  setting <- newManagerWithSocksProxy ("127.0.0.1", 7890)
  putStrLn "Starting"
  resp <-
    requestWith setting $
    setResponseTimeout 0 $
    withJson myData $
    hUtf8json $
    setHeader ("Authorization", "Bearer " <> fromString key) $
    mpost $ "https://api.openai.com/v1/audio/speech"
  let lazyStr = responseBody $ resp
    -- L.putStr lazyStr
  L.writeFile "speech.mp3" lazyStr
  putStrLn "Done"

functionCalling :: IO ()
functionCalling = do
  key <- getEnv "API_KEY"
  setting <- newManagerWithSocksProxy ("127.0.0.1", 7890)
  putStrLn "Starting"
  resp <-
    requestWith setting $
    setResponseTimeout 0 $
    withJson myData $
    hUtf8json $
    setHeader ("Authorization", "Bearer " <> fromString key) $
    mpost $ "https://api.openai.com/v1/chat/completions"
  let lazyStr = responseBody $ resp
  L.putStr lazyStr
  putStrLn "Done"
  where
    myData =
      object
        [ "model" .= s "gpt-3.5-turbo-0125"
        , "messages" .=
          [ object ["role" .= s "system", "content" .= s "你是我的个人助理."]
          , object ["role" .= s "user", "content" .= s "今天的天气如何"]
          , object ["role" .= s "user", "content" .= s "北京的天气"]
          , object
              [ "role" .= s "assistant"
              , "tool_calls" .=
                [ object
                    [ "id" .= s "call_7oMhp077AUrtgdQcJcbnd1yy"
                    , "type" .= s "function"
                    , "function" .=
                      object
                        [ "name" .= s "get_current_weather"
                        , "arguments" .=
                          s "{\"location\":\"Beijing\",\"unit\":\"摄氏度\"}"
                        ]
                    ]
                ]
              ]
          , object
              [ "role" .= s "tool"
              , "tool_call_id" .= s "call_7oMhp077AUrtgdQcJcbnd1yy"
              , "name" .= s "get_current_weather"
              , "content" .= s "大雪 1摄氏度"
              ]
          ]
        , "tools" .=
          [ object
              [ "type" .= s "function"
              , "function" .=
                object
                  [ "name" .= s "get_current_weather"
                  , "description" .= s "获取天气信息"
                  , "parameters" .=
                    object
                      [ "type" .= s "object"
                      , "properties" .=
                        object
                          [ "location" .=
                            object
                              ["type" .= s "string", "description" .= s "城市"]
                          , "unit" .=
                            object
                              ["type" .= s "string", "enum" .= [s "摄氏度", "华氏度"]]
                          ]
                      , "required" .= [s "location"]
                      ]
                  ]
              ]
          ]
        ]
    list a = [a]

dall :: IO ()
dall = do
  key <- getEnv "API_KEY"
  setting <- newManagerWithSocksProxy ("127.0.0.1", 7890)
  resp <- requestWith setting $
    setResponseTimeout 0 $
    withJson myData $
    hUtf8json $
    setHeader ("Authorization", "Bearer " <> fromString key) $
    mpost $ "https://api.openai.com/v1/images/generations"
  let lazyStr = responseBody $ resp
  let result = decode lazyStr
  case result of 
    Just a -> L.putStr $ encode (a :: Value)
    Nothing -> L.writeFile "dall.png" lazyStr
  putStrLn ""
  putStrLn "Done"

  where myData = object [ "model" .= s "dall-e-2"
          , "prompt" .=  s "在一个晴朗的早晨，小熊宝宝醒来，发现他的手指头变得黏黏的，有点儿痒。他想起妈妈曾经告诉他，经常吃手会让手指头变得不干净。"
          , "n" .= n 1
          , "size" .= s "1024x1024"
          ]

dallEdit :: IO ()
dallEdit = do
  key <- getEnv "API_KEY"
  setting <- newManagerWithSocksProxy ("127.0.0.1", 7890)
  let req = setResponseTimeout 0 $
        setHeader ("Authorization", "Bearer " <> fromString key) $
        mpost $ "https://api.openai.com/v1/images/edits"
  req' <-
    putFormData [ partFile "image" "/Users/diqye/Desktop/converted-1.png"
    , partFile "mask" "/Users/diqye/Desktop/converted-1-mask.png"
    , partBS "n" "1"
    , partBS "prompt" "增加一只熊"
    , partBS "size" "512x512" 
    ] req
  resp <- requestWith setting req'
  let lazyStr = responseBody $ resp
  let result = decode lazyStr
  case result of 
    Just a -> L.putStr $ encode (a :: Value)
    Nothing -> L.writeFile "dall.png" lazyStr
  putStrLn ""
  putStrLn "Done"

  where myData = object [ "model" .= s "dall-e-2"
          , "prompt" .=  s "The staff is doing a manicure on a girl"
          , "n" .= n 1
          , "size" .= s "1024x1024"
          ]

models :: IO ()
models = do
  key <- getEnv "API_KEY_DOUYU"
  -- setting <- newManagerWithSocksProxy ("127.0.0.1", 7890)
  resp <- request $
    setResponseTimeout 0 $
    setHeader ("Authorization", "Bearer " <> fromString key) $
    mget $ "https://oneapi.zmexing.com/v1/models"
  let lazyStr = responseBody $ resp
  L.putStr lazyStr
  putStrLn ""
main :: IO ()
main = undefined

