--function square
square :: Int -> Int
square x = x * x

--variable
answerToEverything :: Int
answerToEverything = 42

--minimum
myMin :: Int -> Int -> Int
myMin x y = if x > y then y else x

--n!
factorial :: Int -> Int
factorial n = if n == 0 then 1 else n * factorial (n-1)