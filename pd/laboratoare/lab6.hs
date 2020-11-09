import Data.List (nub)
import Data.Maybe (fromJust)

-- Tipul de date care reține fructe
data Fruct
  = -- Măr, care reține soiul, și dacă are un vierme sau nu
    Mar String Bool
  | -- Portocală, care reține soiul și numărul de felii
    Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False

goldenCuVierme = Mar "Golden Delicious" True

portocalaSicilia10 = Portocala "Sanguinello" 10

listaFructe =
  [ Mar "Ionatan" False,
    Portocala "Sanguinello" 10,
    Portocala "Valencia" 22,
    Mar "Golden Delicious" True,
    Portocala "Sanguinello" 15,
    Portocala "Moro" 12,
    Portocala "Tarocco" 3,
    Portocala "Moro" 12,
    Portocala "Valencia" 2,
    Mar "Golden Delicious" False,
    Mar "Golden" False,
    Mar "Golden" True
  ]

-- Verifică dacă soiul dat este de portocală siciliană
eSoiDeSicilia :: String -> Bool
eSoiDeSicilia soi =
  soi
    `elem` [ "Tarocco",
             "Moro",
             "Sanguinello"
           ]

-- Verifică dacă fructul dat este o portocală siciliană
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = eSoiDeSicilia soi
ePortocalaDeSicilia _ = False

-- Numără câte felii de portocale din Sicilia avem în listă
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum [felii | (Portocala soi felii) <- l, eSoiDeSicilia soi]

-- Numără câte mere cu viermi avem în listă
nrMereViermi :: [Fruct] -> Int
nrMereViermi l = sum [1 | (Mar _ True) <- l]

--- Tipuri de date pentru reținerea unor animale
type NumeA = String

type Rasa = String

data Animal = Pisica NumeA | Caine NumeA Rasa

-- Primește un animal și afișează zgomotele produse de acesta
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

-- Returnează rasa animalului dat ca parametru, dacă acesta este un câine
rasa :: Animal -> Maybe String
rasa (Caine _ rs) = Just rs
rasa _ = Nothing

--- Logică propozițională implementată în Haskell
type Nume = String

-- Tip de date pentru reținerea propozițiilor
data Prop
  = Var Nume
  | F
  | T
  | Not Prop
  | Prop :|: Prop
  | Prop :&: Prop
  | Prop :->: Prop
  | Prop :<->: Prop
  deriving (Eq, Read)

-- Funcții de afișare a propozițiilor
instance Show Prop where
  show (Var nume) = nume
  show F = "False"
  show T = "True"
  show (Not p) = "(~" ++ show p ++ ")"
  show (a :|: b) = "(" ++ show a ++ "|" ++ show b ++ ")"
  show (a :&: b) = "(" ++ show a ++ "&" ++ show b ++ ")"
  show (a :->: b) = "(" ++ show a ++ "->" ++ show b ++ ")"
  show (a :<->: b) = "(" ++ show a ++ "<->" ++ show b ++ ")"

infixr 2 :|:
infixr 3 :&:
infixr 4 :->:
infixr 4 :<->:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

-- Reține valorile variabilelor
type Env = [(Nume, Bool)]

-- Caută valoarea unei variabile în evaluare
impureLookup :: Eq a => a -> [(a, b)] -> b
impureLookup a = fromJust . lookup a

-- Calculează valoarea de adevăr a unei propoziții, cu evaluarea dată
eval :: Prop -> Env -> Bool
eval (Var nume) env = impureLookup nume env
eval F _ = False
eval T _ = True
eval (Not p) env = not (eval p env)
eval (a :|: b) env = (eval a env) || (eval b env)
eval (a :&: b) env = (eval a env) && (eval b env)
eval (a :->: b) env = (eval a env) <= (eval b env)
eval (a :<->: b) env = (eval a env) == (eval b env)

-- Returnează lista variabilelor dintr-o propoziție
variabile :: Prop -> [Nume]
variabile = nub . variabile'
  where
    variabile' (Var nume) = [nume]
    variabile' F = []
    variabile' T = []
    variabile' (Not p) = variabile' p
    variabile' (a :|: b) = (variabile' a) ++ (variabile' b)
    variabile' (a :&: b) = (variabile' a) ++ (variabile' b)
    variabile' (a :->: b) = (variabile' a) ++ (variabile' b)
    variabile' (a :<->: b) = (variabile' a) ++ (variabile' b)

-- Generează toate atribuirile posibile pentru o listă de variabile dată
envs :: [Nume] -> [Env]
envs [] = []
envs [x] = [[(x, False)], [(x, True)]]
envs (x : xs) = withFalse ++ withTrue
  where
    rest = envs xs
    withFalse = map (++ [(x, False)]) rest
    withTrue =  map (++ [(x, True)]) rest


-- Returnează toate valorile de adevăr pe care le poate avea o propoziție,
-- pe baza tuturor evaluărilor posibile.
evaluari :: Prop -> [Bool]
evaluari p = map (p `eval`) allEnvs
    where
        variables = variabile p
        allEnvs = envs variables

-- Verifică dacă o propoziție este satisfiabilă (poate fi vreodată `True`)
satisfiabila :: Prop -> Bool
satisfiabila = or . evaluari

-- Verifică dacă o propoziție este validă (este întotdeauna `True`)
valida :: Prop -> Bool
valida = and . evaluari
