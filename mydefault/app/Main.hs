import Mydefault
import Test.QuickCheck
import Data.Aeson


prop_merge a@(Object _) b@(Object _) = a <<>> b  == b <> a
prop_merge a b = a <<>> b  == a && a <> b == a

main :: IO ()
main = do
    quickCheck prop_merge


{-
    [1,2,3]
    [1+2,3] [1+3,2]
    [3+3] [4+2]
-}
compose :: (Eq a,Floating a,Show a) => [a] -> [(a,[String])]
compose [] = [(0,[])] 
compose [x] = [(x,[])]
compose (x:xs) = concat r where
    r =
        map (filter is24) oP  <>
        map (filter is24) oS  <>
        map (filter is24) oS' <>
        map (filter is24) oM  <>
        map (filter is24) oD  <>
        map (filter is24) oD'
    is24 (24,_) = True
    is24 _ = False
    oP = opFn $ \ a b -> (a + b,show a <> "+" <> show b)
    oS = opFn $ \ a b -> (a - b,show a <> "-" <> show b)
    oS' = opFn $ \ a b -> (b - a,show b <> "-" <> show a)
    oM = opFn $ \ a b -> (a * b,show a <> "*" <> show b)
    oD = opFn $ \ a b -> (a / b,show a <> "/" <> show b)
    oD' = opFn $ \ a b -> (b / a,show b <> "/" <> show a)
    opFn fn  = map (myrecur . (\(a,b)->let (na,flag) = fn x a in (na:b,flag)) . flip pluck xs) idxs
    idxs = [0..length xs - 1]
    myrecur (xs,flag) = map (fmap (flag:)) result where
        result = compose xs


pluck :: Int -> [a] -> (a,[a])
pluck n xs = (head a,b <> tail a) where
    a = drop n xs
    b = take n xs
    