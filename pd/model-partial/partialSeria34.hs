-- Funcție care elimină literele duplicate consecutive dintr-un șir.
-- Doar recursie și funcții din categoria A.
elimDuplicCons :: String -> String
elimDuplicCons "" = ""
elimDuplicCons [caracter] = [caracter]
elimDuplicCons (a:b:c) =
    if a == b then
        -- Sărim peste `b` și aplicăm recursiv funcția
        elimDuplicCons (a : c)
    else
        -- Punem `a` (care știm că e distinct), și continuăm recursiv
        -- de la al doilea caracter
        a : elimDuplicCons (b : c)


-- Pentru două list date, calculează suma produselor de forma x_i^2 * y_i^2,
-- cu x_i din x și y_i din y.
-- Pentru liste de lungimi diferite, aruncă o eroare.
-- Doar funcții de nivel înalt.
sumaProdPatrat :: [Int] -> [Int] -> Int
sumaProdPatrat x y =
    if length x /= length y then
        error "Listele au lungimi diferite"
    else
        let
            x2 = map (^2) x
            y2 = map (^2) y
            perechi = zip x2 y2
            produse = map (\(a, b) -> a * b) perechi
        in
            foldr (+) 0 produse


-- Tipuri de date din enunț
data Pereche = P Int Int
    deriving (Show)

data Lista = L [Pereche]
    deriving (Show)

data Exp = I Int | Add Exp Exp | Mul Exp Exp

class ClassExp m where
    toExp :: m -> Exp

-- Instanța clasei definite mai sus pentru tipul dat
instance ClassExp Lista where
    -- Convertește lista de perechi într-o sumă,
    -- în care numerele din fiecare pereche sunt înmulțite între ele.
    toExp (L lista) = toExp' lista
        where
            perecheToExp (P a b) = Mul (I a) (I b)
            toExp' [] = I 0
            toExp' [p] = perecheToExp p
            toExp' (x:xs) = Add (perecheToExp x) (toExp' xs)


-- Afișare pentru expresii
instance Show Exp where
    show (I valoare) = show valoare
    show (Add lhs rhs) = "(" ++ show lhs ++ " + " ++ show rhs ++ ")"
    show (Mul lhs rhs) = "(" ++ show lhs ++ " * " ++ show rhs ++ ")"
