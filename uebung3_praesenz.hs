import Text.XHtml (base, caption)
import Distribution.Simple.Utils (xargs)
import Data.Sequence (Seq(Empty), empty)
--Fold kommt in der Klausur dran!

--1.)
maximum0 :: [Int] ->  Int
maximum0 []      = -2 ^ 63
maximum0 (x:xs)  = max x (maximum0 xs)

maximumfold :: [Int] -> Int
maximumfold = foldr max (-2 ^ 63)
--maximumfold xs = foldr max (-2 ^ 63) xs
--maximumfold = foldl max (-2 ^ 63)

maximum1 :: [Int] -> Int
maximum1 xs  = maximum1' (-2 ^ 63) xs
    where
        maximum1' e []      = e 
        maximum1' e (x:xs)  = maximum1' (max e x) xs

--2.)
--data Maybe a = Nothing -- :: Maybe a
--             | Just a -- :: a -> Maybe a


-- foldMaybe [] (: []) Nothing -> []
foldMaybe :: b -> (a -> b) -> Maybe a -> b 
foldMaybe nothing just maybe = 
    case maybe of
        Nothing -> nothing
        Just value -> just value

foldEither :: (a -> c) -> (b -> c) -> Either a b -> c 
foldEither left right either =
    case either of
        Left x -> left x
        Right x -> right x 

--3.)
data Tree a b = Empty               -- :: Tree a b
              | Leaf a              -- :: a -> Tree a b
              | Node b [Tree a b]   -- :: b -> [Tree a b] -> Tree a b
    deriving Show

foldTree :: r -> (a -> r) -> (b -> [r] -> r) -> Tree a b -> r
foldTree empty leaf node tree =
    case tree of
        Empty -> empty
        Leaf a -> leaf a
        Node a ts -> node a (map foldTree' ts)
    where
        foldTree' = foldTree empty leaf node
