-- Funktion zur Berechnung der Potenzkombinationen
powers :: Integer -> Integer -> [Integer]
powers base limit = takeWhile (<= limit) $ iterate (* base) 1

-- Einfache Sortierfunktion
simpleSort :: [Integer] -> [Integer]
simpleSort [] = []
simpleSort (x:xs) = simpleSort [y | y <- xs, y <= x] ++ [x] ++ simpleSort [y | y <- xs, y > x]

-- Funktion zur Entfernung von Duplikaten in einer Liste
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) | elem x xs = removeDuplicates xs
                       | otherwise = x : removeDuplicates xs

-- Funktion zur Generierung aller Avalanche-Zahlen
avalanche :: [Integer]
avalanche = simpleSort $ removeDuplicates [result | i <- powers 5 (2^23),
                                                    j <- powers 7 (2^23),
                                                    k <- powers 11 (2^23),
                                                    let result = i * j * k,
                                                    result <= (2^23)]