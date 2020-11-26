import Data.Char (toLower)


-- Funcție care afișează lista de sufixe
sufixe :: String -> [String]

-- Cazul de bază: șirul vid are ca sufix doar subșirul vid
sufixe "" = [""]

-- Cazul recursiv: cuvântul în sine este propriul său sufix,
-- apoi includem sufixele cuvântului fără prima literă.
sufixe cuvant = cuvant : sufixe (tail cuvant)


-- Pentru două liste x și y, calculez produsul dintre termenii de forma
-- (x_i)^2 - (y_i)^2 + 2 * (x_i) * (y_i), cu x_i și y_i din x și y
prodListe :: Num a => [a] -> [a] -> a
prodListe x y =
    let
        -- Parcurg în paralel listele
        perechi = zip x y
        -- Calculul pe care îl fac pentru fiecare x_i și y_i
        operatie (xi, yi) = (xi * xi) - (yi * yi) + 2 * xi * yi
        -- Aplic operația pe toți termenii
        termeni = map operatie perechi
    in
        -- Calculez produsul termenilor
        foldr (*) 1 termeni


type Name = String
type Quantity = Int

data Ingredient = Ing Name Quantity
    deriving Show

data Receipe = R [Ingredient]
    deriving Show

r1 = R [Ing "faina" 500, Ing "oua" 4, Ing "zahar" 500]

r2 = R [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 4]

r3 = R [Ing "fAIna" 500, Ing "zahar" 500, Ing "Oua" 55]


-- Convertește în litere mici un șir.
lowerCaseString :: String -> String
lowerCaseString = map toLower

-- Compară două șiruri pentru egalitate,
-- fără să țină cont de litere mici sau mari.
caseInsensitiveEq :: String -> String -> Bool
caseInsensitiveEq a b = lowerCaseString a == lowerCaseString b

instance Eq Ingredient where
    (Ing name1 quant1) == (Ing name2 quant2) =
        caseInsensitiveEq name1 name2
        && (quant1 == quant2)


-- Șterge prima apariție a unui ingredient dintr-o listă
stergePrimaAparitie :: Ingredient -> [Ingredient] -> [Ingredient]
stergePrimaAparitie _ [] = []
stergePrimaAparitie e (x:xs) =
    -- L-am găsit, îl șterg și nu îl mai caut
    if e == x then xs
    -- Nu l-am găsit, îl las în listă și continui
    else x : stergePrimaAparitie e xs

-- Compară listele de ingrediente, fără să ia în considerare ordinea
comparaListeIng :: [Ingredient] -> [Ingredient] -> Bool
comparaListeIng [] [] = True
comparaListeIng [] _ = False
comparaListeIng (x:xs) ing =
    let
        -- Verific dacă `x` apare în listă
        xEsteInLista = elem x ing
        -- Șterg o apariție a lui `x`, dacă se poate
        listaFaraPrimulX = stergePrimaAparitie x ing
    in
        xEsteInLista && comparaListeIng xs listaFaraPrimulX

instance Eq Receipe where
    (R ing1) == (R ing2) =
        comparaListeIng ing1 ing2
