import Control.Lens
import Data.Aeson
import qualified Data.Yaml as Y
import System.IO(readFile)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L


data Detail = Detail {
    d_1 :: Int
} deriving Show
data User = User {
  name :: String,
  age :: Int,
  detail :: Detail
} | Other {
    ida :: String
} deriving(Show)

ageLens :: Lens' User Int
ageLens = lens age (\user newAge -> user { age = newAge })

detailLens :: Lens' User Detail
detailLens = lens detail (\user newd -> user { detail = newd })


d_1Lens :: Lens' Detail Int
d_1Lens = lens d_1 (\detail newd -> Detail{
    d_1 = newd
})

myuser = User {
    name = "hello",
    age = 22,
    detail = Detail {
        d_1 = 100
    }
}

myother = Other {
    ida = "ida"
}

yaml :: IO ()
yaml = do
    yaml <- B.readFile "./test.yaml"
    case Y.decodeEither' yaml of
        Right v -> do
            L.putStrLn $ encode (v::Value)
        Left e -> print e


main :: IO ()
main = do
    print user1
    where user1 = Other {
        ida = "dd"
    }
        