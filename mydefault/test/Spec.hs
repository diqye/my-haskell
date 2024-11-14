import Test.QuickCheck
import Mydefault
import Data.Aeson
import Data.Text

instance Arbitrary Text where
    arbitrary = pack <$> arbitrary

prop_merge :: Value -> Value -> Bool
prop_merge a1@(Object o1) a2@(Object o2) = a1 <> a2 == Object (o1 <> o2)
prop_merge a1@(String t1) a2@(String t2) = a1 <> a2 == String (t1 <> t2)
prop_merge a1@(Array v1) a2@(Array v2) = a1 <> a2 == Array (v1 <> v2)
prop_merge a1 a2 = a1 <> a2 == a1

main :: IO ()
main = do
    putStrLn "test prop_merge"
    quickCheck prop_merge