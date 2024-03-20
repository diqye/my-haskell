{-# LANGUAGE FlexibleInstances #-}
import Mydefault
import Data.Text
-- 这里会覆盖 Mydefault里面的default配置
default(Text,Int)

main :: IO ()
main = do
    putStrLn "helloworld"