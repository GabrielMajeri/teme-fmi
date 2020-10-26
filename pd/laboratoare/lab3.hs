factori :: Int -> [Int]
factori n = [ d | d <- [1..n], n `mod` d == 0 ]

prim :: Int -> Bool
prim n = factori n == [1, n]

numerePrime :: Int -> [Int]
numerePrime n = [ p | p <- [2..n], prim p ]

myzip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xs) (y:ys) (z:zs) = (x, y, z):(myzip3 xs ys zs)


firstEl :: [(a, b)] -> [a]
firstEl = map fst

sumList :: [[Int]] -> [Int]
sumList = map sum

prel2 :: [Int] -> [Int]
prel2 = map prelucreaza
    where
        prelucreaza x =
            if x `mod` 2 == 0
                then x `div` 2
                else x * 2


containsChar :: Char -> [String] -> [String]
containsChar char = filter (elem char)

patrateImpare :: [Int] -> [Int]
patrateImpare = map (^2) . filter odd

patratePozitiiImpare :: [Int] -> [Int]
patratePozitiiImpare = map ((^2) . snd) . filter (odd . fst) . zip [0..]

numaiVocale :: [String] -> [String]
numaiVocale = map removeConsonants
    where
        isVowel :: Char -> Bool
        isVowel ch = elem ch "aeiouAEIOU"
        removeConsonants :: String -> String
        removeConsonants = filter isVowel


mymap :: (a -> b) -> [a] -> [b]
mymap f a = [f(x) | x <- a]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f a = [x | x <- a, f(x)]


numerePrimeCiur :: Int -> [Int]
numerePrimeCiur n = ciurRec [2..n]
    where
        ciurRec [] = []
        ciurRec (x:xs) = x : (ciurRec $ filter (\n -> n `rem` x /= 0) xs)

-- Verifică că o listă de numere este sortată natural.
--
-- Compară fiecare două numere consecutive și se asigură că se respectă
-- x0 < x1 < x2 < ... < xn
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [_] = True
ordonataNat l = and [a < b | (a, b) <- zip l (tail l)]

-- La fel ca mai sus, dar compară recursiv elementele consecutive.
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [_] = True
ordonataNat1 (x:xs) =
    let
        y = head xs
    in
        x < y && ordonataNat1 xs

-- La fel ca mai sus, dar compară folosind relația de ordine dată.
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata l rel = and [rel a b | (a, b) <- zip l (tail l)]

(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
(a, b) *<* (c, d) = a < c && b > d


-- Compune o funcție cu o listă de funcții
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList f lf = map (f .) lf

-- Aplică o listă de funcții pe aceeași valoare
aplicaList :: a -> [(a -> b)] -> [b]
aplicaList valoare lista = map (\f -> f valoare) lista


-- Folosim zip pentru a obține o listă de perechi de pereche și element,
-- apoi o transformăm într-o listă de tiplete
myzip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip3' a b c = map despacheteaza elemente
    where
        despacheteaza (x, (y, z)) = (x, y, z)
        elemente = zip a (zip b c)
