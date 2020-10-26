import Numeric.Natural (Natural)

-- Calculează produsul unei liste de numere
produsRec :: [Integer] -> Integer
produsRec [] = 1
produsRec (x:xs) = x * (produsRec xs)

produsFold :: [Integer] -> Integer
produsFold = foldr (*) 1


-- Calculează conjuncția unei liste de valori boolene
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && (andRec xs)

andFold :: [Bool] -> Bool
andFold = foldr (&&) True


-- Concatenează o listă de liste
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ (concatRec xs)

concatFold :: [[a]] -> [a]
concatFold = foldr (++) []


-- Elimină toate aparițiile unui caracter dintr-un șir
rmChar :: Char -> String -> String
rmChar ch = filter (/= ch)

-- Elimină toate aparițiile unor caractere (primul parametru)
-- dintr-un alt șir (al doilea parametru)
rmCharsRec :: String -> String -> String
rmCharsRec "" = id
rmCharsRec (x:xs) = rmChar x . rmCharsRec xs

rmCharsFold :: String -> String -> String
rmCharsFold caractere input = foldr rmChar input caractere


-- Funcția logistică, cu o anumită rată de creștere și un punct inițial
logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
    where
        f 0 = start
        f n = rate * f (n - 1) * (1 - f (n - 1))

-- Funcția logistică cu parametrii fixați
logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079


-- Valoarea lui `ex1` trebuie să fie suficient de mare ca `logistic0 ex`
-- să dureze mai mult timp (să fie vizibil cât de ineficient e).
ex1 :: Natural
ex1 = 18


-- Pentru a afișa lista, se va executa apelul încet
ex20 :: Fractional a => [a]
ex20 = [1, logistic0 ex1, 3]

-- Pentru primul element, nu e necesar să calculăm funcția logistică
ex21 :: Fractional a => a
ex21 = head ex20

-- Nici pentru ultimul element nu e necesar să calculăm funcția logistică
ex22 :: Fractional a => a
ex22 = ex20 !! 2

-- Ignorăm primele două elemente, deci nu mai calculăm funcția logistică
ex23 :: Fractional a => [a]
ex23 = drop 2 ex20

-- Ignorăm primul element, dar avem nevoie de ultimele două,
-- deci calculăm funcția logistică
ex24 :: Fractional a => [a]
ex24 = tail ex20


-- Avem mai întâi o comparație, apoi apelul funcției
ex31 :: Natural -> Bool
ex31 x = x < 7 || logistic0 (ex1 + x) > 2

-- Avem mai întâi apelul funcției, apoi comparație
ex32 :: Natural -> Bool
ex32 x = logistic0 (ex1 + x) > 2 || x < 7

-- Nu mai evaluează funcția, e True din prima comparație
ex33 :: Bool
ex33 = ex31 5

-- O să evalueze funcția
ex34 :: Bool
ex34 = ex31 7

-- O să apeleze funcția
ex35 :: Bool
ex35 = ex32 5

-- O să apeleze funcția
ex36 :: Bool
ex36 = ex32 7
