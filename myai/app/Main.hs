import Data.Default.Class ( Default(def) )
import Myai.Data.Config (gpt, azure, MonadAI)
import qualified Myai.Data.Azure as A
import Control.Lens
import Control.Monad.Cont (MonadIO(liftIO))
import Data.Aeson (object, encode)
import Data.Aeson.Lens
import Mydefault
import Myai
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)

test :: IO ()
test = do
    r <- runAIT (setConfig def) testAI
    print r
    where
        setConfig =
            (azure . A.key .~ "9c0c" ) .
            (azure . A.endpoint .~ "xxxx")


testAI :: (MonadIO m ,MonadAI m) => m ()
testAI = do
    req <- useAzureRequest "gpt-4o"
    let param = object [
                "temperature" =: 0,
                "stream" =: True,
                "messages" =: [
                    object ["role" =: "system", "content" =: "你是苏东坡"],
                    object ["role" =: "user", "content" =: "你好"]
                ]
            ]
    
    useStream param req $ do
        (recur,value,_) <- recurValue ()
        liftIO $ T.putStr $ fromMaybe "" $ value  ^? key "choices" . nth 0 . key "delta" . key "content" . _JSON'
        recur ()
        liftIO $ putStrLn "\nDone"

main :: IO ()
main = putStrLn "main"