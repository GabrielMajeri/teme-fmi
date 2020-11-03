import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum, toUpper)


-- Permută circular primele `N` caractere dintr-un string.
rotate :: Int -> [Char] -> [Char]
rotate n str
    | n < 0 = error "n is negative"
    | n > length str = error "n is greater than the string's length"
    | otherwise = (drop n str) ++ (take n str)


-- Alfabetul de intrare, care constă în toate literele mari ale alfabetului englez
alphabet :: String
alphabet = ['A'..'Z']


-- Generează cheia de criptare, o listă de perechi care indică
-- ce caracter se duce în care
makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabet rotated
    where
        rotated = rotate n alphabet

-- Fiind dat un caracter și o cheie de criptare,
-- găsește caracterul criptat corespunzător.
--
-- Returnează caracterul dat ca parametru dacă nu se poate cripta.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char key =
    let
        -- Căutăm perechea corespunzătoare caracterului dat.
        wantedPair = find (\(a, _) -> a == char) key
        -- Încercăm să extragem caracterul criptat corespunzător,
        -- dacă a fost găsită perechea.
        wantedChar = fmap snd wantedPair
    in
        -- Dacă am găsit caracterul criptat îl returnăm,
        -- dacă nu returnăm caracterul inițial, necriptat.
        fromMaybe char wantedChar


-- Elimină din șir caracterele care nu sunt litere mari sau cifre.
normalize :: String -> String
normalize = map toUpper . filter isAlphaNum


-- Criptează un caracter pentru un anumit N
encipher :: Int -> Char -> Char
encipher n char = lookUp char (makeKey n)

-- Criptează întregul șir normalizat dat ca paremtru, pentru un anumit N
encipherStr :: Int -> String -> String
encipherStr n = map (encipher n) . normalize


-- Pentru decriptare, ne folosim de proprietățile permutărilor circulare.
-- O rotație de lungime M este inversată de una de lungime N - M.
decipher :: Int -> Char -> Char
decipher n = encipher (length alphabet - n)

-- Decriptează tot șirul, ca mai sus
decipherStr :: Int -> String -> String
decipherStr n = map (decipher n) . normalize
