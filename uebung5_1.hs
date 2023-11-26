import Data.Ratio

-- Goldener Schnitt
goldenRatio :: [Rational]
goldenRatio = map (\n -> 1 + fib n % fib (n + 1)) [0..]
  where
    fib n = fibonacci !! n

--Approximationsfkt
approx :: Rational -> [Rational] -> Rational
approx _ [] = error "Liste der Approximationen ist leer"
approx _ [_] = error "Es werden mindestens zwei Approximationen benötigt"
approx eps (x1:x2:xs)
  | abs (x1 - x2) <= eps = x2
  | otherwise = approx eps (x2:xs)

-- Fibonacci-Hauptfkt
fibonacci :: [Integer]
fibonacci = fibonacci' 0 1
    where
        fibonacci' :: Integer -> Integer -> [Integer]
        fibonacci' a b = a : fibonacci' b (a + b)
