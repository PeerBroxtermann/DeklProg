--2) Suchbäume

data SearchTree = Empty | Node Int SearchTree SearchTree
  deriving Show

--Insert given Integer into given SearchTree
insert :: Int -> SearchTree -> SearchTree
insert x Empty = Node x Empty Empty
insert x (Node n left right)
  | x < n     = Node n (insert x left) right
  | x > n     = Node n left (insert x right)
  | otherwise = Node n left right --if number already exists, it is not added again

--checks if a given Integer is contained in a given Search Tree
isElem :: Int -> SearchTree -> Bool
isElem _ Empty = False
isElem x (Node n left right)
  | x < n     = isElem x left
  | x > n     = isElem x right
  | otherwise = True

--Deletes given Value from given Search Tree
delete :: Int -> SearchTree -> SearchTree
delete _ Empty = Empty
delete x (Node n left right)
  | x < n     = Node n (delete x left) right
  | x > n     = Node n left (delete x right)
  | otherwise = deleteNode (Node n left right)
  where
    findMin :: SearchTree -> Int
    findMin Empty            = error "Der Baum ist leer"
    findMin (Node y Empty _) = y
    findMin (Node _ left1 _)  = findMin left1

    deleteNode :: SearchTree -> SearchTree
    deleteNode Empty                 = error "Der Baum ist leer"
    deleteNode (Node _ left2 Empty)  = left2
    deleteNode (Node _ Empty right2) = right2
    deleteNode (Node _ left2 right2) = Node minVal left newRight
      where
        minVal = findMin right2
        newRight = delete minVal right2