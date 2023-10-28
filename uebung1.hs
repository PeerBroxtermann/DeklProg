--Praesenzaufgaben

--IntSum Gauss
intSum1 :: Int -> Int 
intSum1 n = div (n * (n+1)) 2

--IntSum Recursive
intSum2 :: Int -> Int 
intSum2 n = if n == 0 then 0 else n + intSum2(n-1)


--1) Screenshot

--2) Pascalsches Dreieck

--n!
factorial :: Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n-1)

--Binomialkoeffizient
binom :: Int -> Int -> Int 
binom n k = div (factorial n) (factorial k * factorial (n-k))

--Pascalsches Dreieck als rekursive Funktion
pascal :: Int -> Int -> Int 
pascal row pos = if pos == 0 || pos == row then 1 else pascal (row-1) pos + pascal (row-1) (pos-1)

--3) Zweierpotenzen

--Zweierpotenz
nthPowerTwo :: Int -> Int 
nthPowerTwo x = 2 ^ x


--Ist Zweierpotenz?
isPowerTwo :: Int -> Bool
isPowerTwo n 
    | n < 1     = False
    | n == 1    = True
    | otherwise = isPowerTwo(n `div` 2) && even n

--Rundet auf zur nächsten Zweierpotenz
roundUpToPowerTwo :: Int -> Int 
roundUpToPowerTwo n
    | n == 1    = 1
    | otherwise = if isPowerTwo n then n else roundUpToPowerTwo (n+1)