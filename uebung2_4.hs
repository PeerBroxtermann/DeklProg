-- 4) Listenfunktionen

--Reverses a List without helper functions
reverse1 :: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]
--Laufzeit von O(n²)

--Reverses a list in linear time O(n)
reverse2 :: [a] -> [a]
reverse2 xs = reverseAcc xs []
    where
        reverseAcc :: [a] -> [a] -> [a]
        reverseAcc [] acc     = acc
        reverseAcc (y:ys) acc = reverseAcc ys (y:acc)

--Returns the Index of the given element in the given list or Nothing, if elem
--is not contained in the list
indexOf :: Int -> [Int] -> Maybe Int
indexOf _ [] = Nothing
indexOf a (x:xs)
    | a == x = Just 0
    | otherwise = fmap increment (indexOf a xs)
    where
        increment n = n + 1 

--Gibt alle Anfangsstücke von der gegebenen Liste zurück
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

--Gibt alle Endstücke von der gegebenen Liste zurück
tails :: [a] -> [[a]]
tails []     = [[]]
tails (x:xs) = (x:xs) : tails xs

--fügt gegebenes Element an jeder Stelle der Liste ein
insert :: a -> [a] -> [[a]]
insert a []     = [[a]]
insert a (x:xs) = (a:x:xs) : map (x:) (insert a xs)

--Alle Permutationen der gegebenen Liste
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (insert x) (perms xs)