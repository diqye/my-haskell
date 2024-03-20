{-# LANGUAGE DeriveGeneric,QuasiQuotes #-}

import AI.ChatGPT
import System.Environment (getEnv)
import qualified HTTP.Myrequest as H
import Control.Monad.IO.Class(liftIO)
import qualified Data.ByteString.Lazy as L
import Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Data.Default.Class
import AI.Datas
import Control.Lens hiding ((.=))
import Text.RawString.QQ(r)
import Data.Aeson.Lens
import Control.Monad(forM_)

run :: GPT a -> IO (Either [Value] a)
run gpt = do
    key <- getEnv "API_KEY"
    let config = def {
        _c_apiKey = key ,
        _c_baseUrl = "https://api.openai.com/v1/" ,
        _c_managerAction = H.newManagerWithSocksProxy ("127.0.0.1", 7890)
    }
    runGPT  gpt config

runAli :: GPT a -> IO (Either [Value] a)
runAli gpt = do
    key <- getEnv "T_API_KEY"
    runGPT gpt $ def {
        _c_other = Just $ OtherConfig {
            _o_ali = Just (key,"https://dashscope.aliyuncs.com/api/v1/")
        }
    }
runD :: GPT a -> IO (Either [Value] a)
runD gpt = do
    key <- getEnv "API_KEY_DOUYU"
    let config = def {
         _c_apiKey = key ,
          _c_baseUrl = "https://oneapi.zmexing.com/v1/" ,
        _c_managerAction = H.newTlsManager
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
streamMain = chatStreamWith "gpt-4" messages options f where
    f mr = do
        e <- mr
        printE e mr
    printE (Left "") _ = putStrLn "\nfinished"
    printE (Left str) mr = do 
        B.putStrLn $ "ERROR------>"<> str
        f mr
    printE (Right a) mr = do
        let content =
                a ^?
                chunk_choices .
                ix 0 .
                chunk_c_delta .
                chunk_c_content
        case content of Just a -> T.putStr a
                        _ -> pure ()
        f mr
    options = A.object [ ]
messages = [
    system [r|
你是词曲作家，用户提供的背景故事编写一首词
    |],
    user "我要和好朋友分别了，请为我写一首歌",
    assistant [r|
思考: 这是我在送别我的好朋友时有感而发
{
name: "送别",
ps:[{
        output:[[3,"长亭外"],[3,"古道边"],[5,"芳草碧连天"]],
        debug: "三个字一组，绵延悠长,最后五字收尾"
    },{
        output: [[7,"晚风拂柳笛声残"],[5,"夕阳山外山"]],
        debug: "7字开头，5字收尾"
    },{
        output:[[3,"天之涯"],[3,"地之角"],[5,"知交半零落"]],
        debug: "三个字一组，绵延悠长,最后五字收尾"
    },{
        output: [[7,"一觚浊酒尽余欢"],[,5"今宵别梦寒"]],
        debug: "7字开头，5字收尾"
    },{
        output:[[3,"长亭外"],[3,"古道边"],[5,"芳草碧连天"]],
        debug: "三个字一段，绵延悠长,最后五字收尾"
    },{
        output: [[7,"问君此去几时来"],"[5,来时莫徘徊"]],
        debug: "7字开头，5字收尾"
    },{
        output:[[3,"天之涯"],[3,"地之角"],[5,"知交半零落"]],
        debug: "三个字一段，绵延悠长,最后五字收尾"
    },{
        output: [[7,"人生难得是欢聚"],[5,"惟有别离多"]],
        debug: "7字开头，5字收尾"
    }]
}

    |],
    user [r|
注意上面数据格式中的[number,string]元祖，数字表示一共几个字,如[3,"长亭外"] 3是一共三个字，这三个字是 "长亭外".
用一致的格式按照下面故事从新写一首词，只能更改output里面的字和debug内容，output中不要改变字数。
背景故事:
随意发挥吧
    |]
    ]
aliChatMain :: GPT () 
aliChatMain = do
    a <- aliChatWith qwenTurbo messages defOption  
    let content = a ^?!
            key "output" .
            key "choices" .
            nth 0 .
            key "message" .
            key "content" .
            _JSON'
    liftIO $ putStrLn content
aliChatVlMain :: GPT ()
aliChatVlMain = do
    a <- aliChatWith qwenVlPlus msg defOption  
    let content = a ^?!
            key "output" .
            key "choices" .
            nth 0 .
            key "message" .
            key "content" .
            nth 0 . 
            key "text" .
            _JSON'
    liftIO $ putStrLn content
    where msg = [
                vlMsg "user" [
                    MImage "https://devstatic.douyuxingchen.com/hash/diqye/4b501962c021d4f38a36933925690716b6372fa5.png",
                    MText "这张图片有什么"
                ]
            ]
chatMain :: GPT ()
chatMain = do 
    -- gpt-3.5-turbo
    a <- chatWith "gpt-4-32k" messages defOption
    let content =
            a ^?!
            chat_choices .
            ix 0 .
            choice_message .
            msg'content
    liftIO $ T.putStrLn content

-- gpt-4-vision-preview
-- gpt-4-1106-preview
-- gpt-4-turbo-preview
chatWithImageMain :: GPT ()
chatWithImageMain = do
    a <- chat "gpt-4-turbo-preview" messages
    let content =
            a ^?!
            chat_choices .
            ix 0 .
            choice_message .
            msg'content
    liftIO $ T.putStrLn content
    where messages = 
            [ userImage
                [ MImage "https://devstatic.douyuxingchen.com/hash/diqye/4b501962c021d4f38a36933925690716b6372fa5.png"
                , MText "这张图片有什么"
                ]
            ]
imageMain :: GPT ()
imageMain = do
    v <- imageGenerate $ def & img'prompt .~ "一个玩耍的孩子"
    let datas = v ^..  key "data" . values 
    forM_ (datas::[Value]) $ \ img -> do
        liftIO $ T.putStrLn $ img ^?! key "url" . _String

speechMain :: GPT ()
speechMain = do
    mp3 <- speech $ speechOption "你好，我在测试你"
    liftIO $ L.writeFile "test.mp3" mp3

aliImageMain :: GPT ()
aliImageMain = do
    value <- aliImageGenerate $ createAliImageOptions Nothing p
    liftIO $ L.putStrLn $ encode value
    where p = "少女,红色，举起双手" :: T.Text
aliTaskMain :: String -> GPT ()
aliTaskMain taskId = do
    value <- aliTask taskId
    liftIO $ L.putStrLn $ encode value
main :: IO ()
main = do
--    a <- run models
--    a <- runD (retrieveModel "dall-e--3")
    -- a <- runD (chatWith "gpt-3.5-turbo" [Message ("user","hello")] (A.object []))
    a <- runD streamMain
    printValue $ a
    
    
