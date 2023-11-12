data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show, Eq)

-- flatTree: Flattens a tree of trees into a tree of elements
flatTree :: Tree (Tree a) -> Tree a
flatTree (Leaf t) = t
flatTree (Branch left right) = Branch (flatTree left) (flatTree right)

-- mapTree: Applies a function to each element in the tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Branch left right) = Branch (mapTree f left) (mapTree f right)

-- foldTree: Folds a tree into a single value using given functions
foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leafFunc branchFunc (Leaf a) = leafFunc a
foldTree leafFunc branchFunc (Branch left right) = branchFunc (foldTree leafFunc branchFunc left) (foldTree leafFunc branchFunc right)

-- extendTree: Extends a tree at the leaves using a given function
extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree f (Leaf a) = f a
extendTree f (Branch left right) = Branch (extendTree f left) (extendTree f right)

-- Beispiele für die Nutzung:
ex1, ex2, ex3, ex4 :: Bool
ex1 = flatTree (Branch (Leaf (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3)))) (Leaf (Leaf 4))) == Branch (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))) (Leaf 4)
ex2 = mapTree (*2) (Branch (Leaf (-2)) (Leaf 1)) == Branch (Leaf (-4)) (Leaf 2)
ex3 = foldTree id max (Branch (Leaf 42) (Leaf 72)) == 72
ex4 = extendTree Leaf (Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1)) == Branch (Branch (Leaf 3) (Leaf 2)) (Leaf 1)
