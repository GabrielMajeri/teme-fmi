-- Fibonacci în timp liniar, folosind tupluri
fibonacciPereche :: Integer -> (Integer, Integer)
fibonacciPereche 1 = (0, 1)
fibonacciPereche n =
    let (a, b) = fibonacciPereche (n - 1)
    in (b, a + b)

-- Fibonacci în timp liniar, folosindu-se de funcția definită mai sus
fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)


-- Elimină numerele impare din listă și le înjumătățește pe cele pare
semiPare :: [Int] -> [Int]
semiPare = map (\x -> x `div` 2) . filter even

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]


-- Funcții care calculează dublul, triplul, respectiv 5x numărul dat ca parametru.
double :: Integer -> Integer
double = (*) 2
triple :: Integer -> Integer
triple = (*) 3
penta :: Integer -> Integer
penta = (*) 5

testMultiply :: Integer -> Bool
testMultiply x = (double x + triple x) == (penta x)


-- Filtrează din listă numerele care se află într-un interval
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp low high l =
    [ x | x <- l, contains x ]
    where
        contains n = low <= n && n <= high
