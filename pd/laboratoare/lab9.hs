-- Laboratorul 9: funcțiile de input și output --
import Control.Monad (forM, forM_)
import Data.List.Split (splitOn)

---- Exercițiul 1 ----
type Nume = String
type Varsta = Int

-- Tip de date pentru reținerea unei persoane
data Persoana = Pers Nume Varsta
    deriving Eq

instance Ord Persoana where
    -- Compar persoanele prin vârsta lor
    (Pers _ varsta1) <= (Pers _ varsta2) = varsta1 <= varsta2


-- Citește datele unei persoane de la tastatură
citestePersoana :: IO Persoana
citestePersoana = do
    -- Citesc numele persoanei
    putStr "Nume: "
    nume <- getLine
    -- Citesc vârsta persoanei
    putStr "Vârsta: "
    varsta <- readLn :: IO Int
    -- Construiesc obiectul de tip Persoană
    return $ Pers nume varsta


-- Citește datele a N persoane de la tastatură
citesteNPersoane :: IO [Persoana]
citesteNPersoane = do
    -- Citesc N-ul
    putStr "N = "
    nr <- readLn :: IO Int
    -- Pentru fiecare i de la 0 la N, citesc câte o persoană
    forM [1..nr] $ \_ -> do
        citestePersoana


-- Afișează cea mai bătrână persoană dintr-o listă de persoane
afiseazaCeaMaiBatranaPersoana :: [Persoana] -> IO ()
afiseazaCeaMaiBatranaPersoana persoane = do
    -- Găsesc cea mai bătrână persoană din listă
    let ceaMaiBatrana = maximum persoane
    -- Extrag numele și vârsta
    let (Pers nume varsta) = ceaMaiBatrana
    -- Afișare
    putStr $ "Cel mai în vârstă este " ++ nume
    putStrLn $ " (" ++ show varsta ++ " de ani)"


rezolvaEx1 :: IO ()
rezolvaEx1 = do
    -- Citesc lista de persoane
    persoane <- citesteNPersoane
    -- Rezolv cerința
    afiseazaCeaMaiBatranaPersoana persoane


---- Exercițiul 2 ----
citestePersoaneDinFisier :: IO [Persoana]
citestePersoaneDinFisier = do
    input <- readFile "lab9-ex2.in"
    -- Parcurg linie cu linie datele de intrare
    let linii = lines input
    forM linii $ \linie -> do
        -- Descompun linia după virgulă
        let [nume, varsta] = splitOn "," linie
        -- Contruiesc un nou obiect de tip persoană
        return $ Pers nume (read varsta)

rezolvaEx2 :: IO ()
rezolvaEx2 = do
    -- Citesc persoanele din fișier
    persoane <- citestePersoaneDinFisier
    -- Rezolv cerința
    afiseazaCeaMaiBatranaPersoana persoane


---- Exercițiul 3 ----
estePalindrom :: Int -> Bool
estePalindrom n =
    let
        sir = show n
    in
        sir == reverse sir

citesteVerifPalindrom :: IO ()
citesteVerifPalindrom = do
    -- Citesc
    nr <- readLn
    -- Afișez
    if estePalindrom nr then
        putStrLn $ show nr ++ " este palindrom"
    else
        putStrLn $ show nr ++ " nu este palindrom"

suntPalindroame :: Int -> IO ()
suntPalindroame n =
    -- Pentru i de la 1 la N
    forM_ [1..n] $ \_ -> do
        -- Citesc și verific că numărul este palindrom
        citesteVerifPalindrom
