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
import Control.Concurrent.Async
import Control.Concurrent

main = do
    a <- async $ do
        threadDelay (1000000*2)
        putStrLn "after 2s"
    putStrLn "hello immediately"
    wait a