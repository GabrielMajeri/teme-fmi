-- Funcție care verifică dacă caracterul dat este sfârșit de propoziție
sfChr :: Char -> Bool
sfChr c =
  (c == '.') || (c == '?')
    || (c == '!')
    || (c == ':')

-- Numără câte propoziții se află în șirul dat.
nrProp :: String -> Int
nrProp "" = 0
nrProp (x : xs) = nrProp xs + if sfChr x then 1 else 0

test_nrProp = nrProp "Exemplu! Ana are: mere, pere. Câte fructe are Ana?"


-- Verifică că toate liniile de lungime N din matrice au toate numerele strict pozitive
liniiN :: [[Int]] -> Int -> Bool
liniiN matrix n =
  foldr (&&) True (map isAllPositive rowsOfLengthN)
  where
    rowsOfLengthN :: [[Int]]
    rowsOfLengthN = filter ((== n) . length) matrix
    isAllPositive :: [Int] -> Bool
    isAllPositive row = foldr (&&) True (map (> 0) row)

test_liniiN_1 :: Bool
test_liniiN_1 = liniiN [[1, -2], [3, 5, 2], [1, 4, 1]] 3 == True

test_liniiN_2 :: Bool
test_liniiN_2 = liniiN [[1, -2], [3, 5, 2], [1, 4, 0]] 3 == False


data Punct = Pt [Int]
  deriving (Show)

data Arb = Vid | F Int | N Arb Arb
  deriving (Show)

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

instance ToFromArb Punct where
    -- Transformă un punct într-un arbore binar
    toArb (Pt coord) = coordToArb coord
        where
            coordToArb [] = Vid
            coordToArb (x:xs) = N (F x) (coordToArb xs)
    -- Transformă un arbore generat cu funcția precedentă
    -- înapoi într-un punct
    fromArb arb = Pt (coordFromArb arb)
        where
            coordFromArb Vid = []
            coordFromArb (N (F x) b) = x : coordFromArb b
