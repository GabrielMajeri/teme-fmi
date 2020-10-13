import Data.Char (isDigit, digitToInt)

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
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec low high (x:xs) =
    let
        rest = inIntervalRec low high xs
    in
        if low <= x && x <= high
            then x:rest
            else rest

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp low high l =
    [ x | x <- l, contains x ]
    where
        contains n = low <= n && n <= high


-- Numără câte numere sunt strict pozitive, dintr-o listă primită
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs) = (if x > 0 then 1 else 0) + pozitiveRec xs

pozitiveComp :: [Int] -> Int
pozitiveComp l = sum [1 | x <- l, x > 0]


-- Returnează o listă cu indicii pe care se află numere impare
pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozImp' 0 l
    where
        pozImp' _ [] = []
        pozImp' index (x:xs) =
            let
                rest = pozImp' (index + 1) xs
            in
                if x `mod` 2 == 1
                    then index:rest
                    else rest

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [
    pozitie |
        (pozitie, valoare) <- zip [0..] l,
        valoare `mod` 2 == 1
    ]


-- Calculează produsul cifrelor dintr-un șir de caractere
multDigits :: String -> Int
multDigits = product . map digitToInt . filter isDigit


-- Aplică un discount de 25% pe o listă de valori, și
-- le păstrează pe cele care ajung să fie mai mici de 200.
discount :: [Double] -> [Double]
discount = filter (< 200) . map (* 0.75)
