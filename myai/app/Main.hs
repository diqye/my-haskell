{-# LANGUAGE CPP #-}
import Myai
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad(guard,forM_)
import Control.Lens
import Data.Aeson.Lens
import Data.Monoid
import Control.Monad.Except
import Mydefault

merge :: Ord a => [a] -> [a] -> [a] -> [a]
merge [] [] r = r
merge as [] r = r  ++ as
merge [] bs r = r ++ bs
merge as'@(a:as) bs'@(b:bs) r
    | a <= b = merge as bs' (r++[a])
    | otherwise = merge as' bs (r ++ [b])


mydata = A.object [
        "model" <-- "gpt-3.5-turbo" ,
        "temperature" <-- 0 ,
        "messages" <-- [
            A.object [
                "role" <-- "system",
                "content" <-- "给我出三道英文题"
            ]
        ]
    ]
main1 = chatStream  mydata $ do
    liftIO $ putStrLn "========  start =="
    (recur,vals,_) <- recurValues ""
    forM_ vals $ runReaderT $ do
        val <- ask
        liftIO $ putStr $ maybe "" id $ val ^? key "choices" . nth 0 . key "delta" . key "content" . _JSON'
    recur ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "========  end ===="
    pure ()

main = fmap (const ()) $ runEnvAIT $ do
    catchError main1 $ runReaderT $ do 
        (First err) <- ask
        liftIO $ putStrLn "Error"
        liftIO $ L.putStrLn $ A.encode err
    liftIO $ putStrLn "Done"