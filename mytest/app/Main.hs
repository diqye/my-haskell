-- import Control.Lens
-- import Data.Aeson
-- import qualified Data.Yaml as Y
-- import System.IO(readFile)
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Cont
-- import Control.Monad.Trans.Cont hiding (callCC)
import Control.Monad
import Data.Function

-- label :: MonadCont m =>  a -> m (a -> m b,a)
-- label a = callCC $ \ k -> let go b = k (go,b) in pure (go,a)

asReturnType :: (a -> b) -> b -> b
asReturnType _ b = b

asMType :: a b c d -> c e -> c e
asMType _ b = b

a = pure 1 :: Cont String Int
b = asMType a $ pure 1

main = putStrLn "hello"
-- main = evalContT $ callCC $ \ exit -> do
--     (gotoA,n) <- label 0
--     when (n == 3) $ exit ()
--     pwd <- liftIO $ do
--         putStr "What's your password > "
--         getLine
--     when (pwd /= "123") $ gotoA (n+1)
--     liftIO $ putStrLn "Finished"
    
    