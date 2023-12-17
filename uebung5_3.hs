--Alle Kombinationen
allCombinations :: [a] -> [[a]]
allCombinations [] = [[]]
allCombinations xs = let cycled = cycle xs
                     in [] : map (\n -> take n cycled) [1..]


