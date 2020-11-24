import Data.Char (isUpper, toUpper)
import Data.List (find)
import Test.QuickCheck


-- Dublează un număr
double :: Int -> Int
double = (* 2)

-- Triplează un număr
triple :: Int -> Int
triple = (* 3)

-- Înmulțește cu 5 un număr
penta :: Int -> Int
penta = (* 5)

-- Un test care va fi adevărat mereu
test :: Int -> Bool
test x = (double x + triple x) == penta x

runTest = quickCheck test

-- Un test care va fi fals mereu
testFals :: Int -> Bool
testFals x = triple (double x) == penta x

runTestFals = quickCheck testFals



myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp cheie lista =
  let -- Folosesc funcția de căutare predefinită
      result = find (\(k, _) -> k == cheie) lista
   in -- Dacă rezultatul este `Just pereche` extrag al doilea element din pereche,
      -- altfel returnez `Nothing`.
      fmap snd result

-- Testează funcția myLookUp prin comparație cu implementarea standard
testLookUp :: Int -> [(Int, String)] -> Bool
testLookUp cheie lista = lookup cheie lista == myLookUp cheie lista

testLookUpCond :: Int -> [(Int, String)] -> Property
testLookUpCond n list = n > 0 && n `div` 5 == 0 ==> testLookUp n list

-- La fel ca `myLookUp`, dar capitalizează prima literă din rezultat.
myLookUp' :: Int -> [(Int, String)] -> Maybe String
myLookUp' cheie lista = fmap firstLetterToUpper (myLookUp cheie lista)
  where
    firstLetterToUpper (x : xs) = toUpper x : xs

-- Testează ca `myLookUp` și `myLookUp'` să fie echivalente pe listele
-- în care toate valorile încep cu litere mari.
testMyLookUp' :: Int -> [(Int, String)] -> Property
testMyLookUp' n list = doarValoriCuMajuscula list ==> myLookUp' n list == myLookUp n list
  where
    -- Vrem doar liste în care toate elementele încep cu majusculă.
    incepeCuMajuscula sir = sir /= "" && isUpper (head sir)
    doarValoriCuMajuscula l = all (\(_, v) -> incepeCuMajuscula v) l


-- Implementare validă a metodei quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = smalls ++ [x] ++ bigs
  where
    smalls = quicksort [n | n <- xs, n <= x]
    bigs = quicksort [n | n <- xs, n > x]

-- Teste pentru metoda de mai sus
testQuicksort1 :: [Int] -> Bool
testQuicksort1 l = quicksort (quicksort l) == quicksort l

testQuicksort2 :: [Int] -> Bool
testQuicksort2 l = quicksort l == quicksort (reverse l)

testQuicksort3 :: Int -> Bool
testQuicksort3 n = quicksort [1 .. n] == [1 .. n]

testQuicksort4 :: [Int] -> Bool
testQuicksort4 l = length l == length (quicksort l)


-- Implementare cu un bug
quicksortBuggy :: Ord a => [a] -> [a]
quicksortBuggy [] = []
quicksortBuggy (x : xs) = smalls ++ [x] ++ bigs
  where
    smalls = quicksortBuggy [n | n <- xs, n < x] -- oops
    bigs = quicksortBuggy [n | n <- xs, n > x]

-- Teste pentru quicksort cu bug-uri
testQuicksortBuggy1 :: [Int] -> Bool
testQuicksortBuggy1 l = quicksortBuggy (quicksortBuggy l) == quicksortBuggy l

testQuicksortBuggy2 :: [Int] -> Bool
testQuicksortBuggy2 l = quicksortBuggy l == quicksortBuggy (reverse l)

testQuicksortBuggy3 :: Int -> Bool
testQuicksortBuggy3 n = quicksortBuggy [1 .. n] == [1 .. n]

testQuicksortBuggy4 :: [Int] -> Bool
testQuicksortBuggy4 l = length l == length (quicksortBuggy l)



data ElemIS = I Int | S String
  deriving (Show, Eq)

-- Implementare bazată pe https://stackoverflow.com/a/16440400/5723188
instance Arbitrary ElemIS where
  arbitrary = do
    -- Generez un bool random
    choice <- arbitrary
    x <- arbitrary
    y <- arbitrary
    -- În 50% din cazuri voi genera un întreg random,
    -- în celelalte un string random
    return $ if choice then I x else S y


myLookUpElem :: Int -> [(Int, ElemIS)] -> Maybe ElemIS
myLookUpElem cheie lista =
  let result = find (\(k, _) -> k == cheie) lista
   in fmap snd result

testLookUpElem :: Int -> [(Int, ElemIS)] -> Bool
testLookUpElem n lista = myLookUpElem n lista == lookup n lista
