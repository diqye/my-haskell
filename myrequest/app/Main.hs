{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.List (intercalate)
import Data.String (fromString)
import HTTP.Myrequest
import System.IO(readFile)
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
  key <- getEnv "API_KEY"
  msg <- readFile "text.txt"
  let myData = object
        [ "model" .= s "tts-1-hd"
        , "input" .= msg
        , "voice" .= s "onyx"
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

