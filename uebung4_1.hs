type IntegerSet = Integer -> Bool

lostSet :: IntegerSet
lostSet = \x -> elem x [4, 8, 15, 16, 23, 42]

lostSet2 :: IntegerSet
lostSet2 = \x -> elem x [5, 9, 17, 90, 100, 132]

-- leere Menge
empty :: IntegerSet
empty = \_ -> False

-- Zahl zu einer Menge hinzufügen
insert :: Integer -> IntegerSet -> IntegerSet
insert x s = \y -> x == y || s y

-- Zahl aus einer Menge entfernen
remove :: Integer -> IntegerSet -> IntegerSet
remove x s = \y -> x /= y && s y

-- Überprüfen, ob eine Zahl in einer Menge enthalten ist
isElem :: Integer -> IntegerSet -> Bool
isElem x s = s x

-- Vereinigung zweier Mengen
union :: IntegerSet -> IntegerSet -> IntegerSet
union s1 s2 = \x -> s1 x || s2 x

-- Schnitt zweier Mengen
intersection :: IntegerSet -> IntegerSet -> IntegerSet
intersection s1 s2 = \x -> s1 x && s2 x

-- Differenz zweier Mengen
difference :: IntegerSet -> IntegerSet -> IntegerSet
difference s1 s2 = \x -> s1 x && not (s2 x)

-- Komplement einer Menge
complement :: IntegerSet -> IntegerSet
complement s = \x -> not (s x)

-- Konvertierung von Liste zu IntegerSet
listToSet :: [Integer] -> IntegerSet
listToSet lst = \x -> elem x lst

-- Konvertierung von IntegerSet zu Liste
setToList :: IntegerSet -> [Integer]
setToList s = [x | x <- [-1000..1000], s x]