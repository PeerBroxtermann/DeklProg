
--3) Binärbäume

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving Show

--Calculates the sum of all values in a binary tree with only Integer values
sumTree :: Tree Int -> Int
sumTree Empty               = 0
sumTree (Node n left right) = n + sumTree left + sumTree right

--Mirrors a given tree vertically
mirrorTree :: Tree a -> Tree a
mirrorTree Empty                = Empty
mirrorTree (Node n left right)  = Node n (mirrorTree right) (mirrorTree left)

--returns all values in the tree as a list
toList :: Tree a -> [a]
toList Empty                = []
toList (Node x left right)  = x : (toList left ++ toList right)
{--
    Wenn n die Anzahl der Knoten im Baum ist, liegt die Laufzeit in 0(n), da
    jeder Knoten genau ein Mal besucht wird
--}
