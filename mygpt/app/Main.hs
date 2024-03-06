{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
import AI.ChatGPT
import System.Environment (getEnv)
import qualified HTTP.Myrequest as H
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Data.ByteString.Lazy as L
import Data.Aeson as A
import HTTP.Myrequest(s,n)
import qualified Data.ByteString.Lazy.Char8 as L



run :: GPT a -> IO (Either [Value] a)
run gpt = do
    key <- getEnv "API_KEY"
    let config = GPTConfig { api_key = key
        , base_url = "https://api.openai.com/v1/"
        , manager_action = H.newManagerWithSocksProxy ("127.0.0.1", 7890)
        }
    runGPT  gpt config

runD :: GPT a -> IO (Either [Value] a)
runD gpt = do
    key <- getEnv "API_KEY_DOUYU"
    let config = GPTConfig { api_key = key
        , base_url = "https://oneapi.zmexing.com/v1/"
        , manager_action = H.newTlsManager
        }
    runGPT  gpt config

printValue :: (ToJSON a) => Either [Value] a -> IO ()
printValue (Right v) =  do
    L.putStrLn $ encode v
    putStrLn ""
printValue (Left e) = do
    L.putStrLn $ encode e
    putStrLn ""


streamMain :: GPT ()
streamMain = chatStreamWith "gpt-3.5-turbo" [Message (User,"hello")] (A.object []) f where
    f mr = do
        e <- mr
        printE e mr
    printE (Left "") _ = putStrLn "finished"
    printE (Left str) mr = do 
        putStrLn str
        f mr
    printE (Right a) mr = do
        L.putStrLn $ encode a
        f mr
    
chatMain :: GPT ()
chatMain = do 
    a <- chatWith "gpt-3.5-turbo" [Message (User,"你喜欢什么类型的电影？")] (A.object ["top_p" A..= n 0.01]) 
    liftIO $ L.putStrLn $ encode a

main :: IO ()
main = do
--    a <- run models
--    a <- runD (retrieveModel "dall-e--3")
    -- a <- runD (chatWith "gpt-3.5-turbo" [Message ("user","hello")] (A.object []))
    a <- runD streamMain
    printValue $ a
    
    
