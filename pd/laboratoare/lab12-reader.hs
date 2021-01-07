import Control.Monad
import Data.Maybe

--- Monada EnvReader
type Environment = [(Name, Value)]
newtype EnvReader a = Reader { runEnvReader :: Environment -> a }

-- Afișează valoarea conținută în monada identitate
instance Show a => Show (EnvReader a) where
  show x = show $ runEnvReader x []

instance Monad EnvReader where
  -- Pentru orice environment, returnez tot timpul valoarea dată
  return = Reader . const
  ma >>= k = Reader $ \env ->
      let
        -- Aplic prima funcție în environment-ul dat
        value = runEnvReader ma env
        -- Aplic continuarea pe această valoare
        newComp = k value
      in
        -- Evaluez funcția obținută
        runEnvReader newComp env

-- În versiunile mai noi de Haskell trebuie să definesc și instanțe ale acestor clase:
instance Functor EnvReader where
  fmap = liftM
instance Applicative EnvReader where
  pure = return
  (<*>) = ap

-- Întoarce starea curentă ca valoare
ask :: EnvReader Environment
ask = Reader id

-- Modifică environment-ul, și apoi rulează execuția dată
local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma = Reader $ \env -> runEnvReader ma (f env)

--- Limbajul și Interpretorul

-- Prescurtare pentru monada folosită
type M = EnvReader

showM :: Show a => M a -> String
showM = show

type Name = String

data Term
  = Var Name
  | Con Integer
  | Term :+: Term
  | Lam Name Term
  | App Term Term
  deriving (Show)

data Value
  = Num Integer
  | Fun (Value -> M Value)
  | Wrong

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"
  show Wrong = "<wrong>"

-- Calculează rezultatul adunării a două valori.
-- Funcționează doar pentru numere.
add :: Value -> Value -> M Value
add (Num a) (Num b) = return $ Num (a + b)
add _ _ = return Wrong

-- Calculează rezultatul aplicării unei funcții la o valoare.
-- Primul parametru trebuie să fie funcție.
apply :: Value -> Value -> M Value
apply (Fun f) v = f v
apply _ _ = return Wrong

interp :: Term -> M Value

interp (Var name) = do
    -- Citesc environment-ul
    env <- ask
    -- Încerc să găsesc valoarea variabilei
    let maybeValue = lookup name env
    -- Dacă nu pot, returnez `Wrong`
    return $ fromMaybe Wrong maybeValue

-- Returnăm valoarea pentru constanta dată
interp (Con i) = return $ Num i

-- Evaluăm suma a doi termeni
interp (t1 :+: t2) = do
  -- Evaluăm cei doi termeni
  v1 <- interp t1
  v2 <- interp t2
  -- Calculăm suma valorilor lor
  add v1 v2

interp (Lam paramName t) = do
  -- Definim o nouă funcție, care evaluează termenul într-un environment
  -- în care avem o valoare dată pentru parametru
  env <- ask
  return $ Fun $ \x -> local (const ((paramName, x) : env)) (interp t)

-- Aplicăm cei doi termeni
interp (App t1 t2) = do
  -- Le evaluăm valorile
  v1 <- interp t1
  v2 <- interp t2
  -- Aplicăm primul termen cu al doilea
  apply v1 v2

test :: Term -> String
test t = showM $ interp t

-- Ar trebui să afișeze 7
pgm :: Term
pgm =
  App (Lam "y" (
    App (App
          (Lam "f" (Lam "y" (App (Var "f") (Var "y"))))
          (Lam "x" (Var "x" :+: Var "y")))
    (Con 3))
  ) (Con 4)

-- Ar trebui să afișeze 42
pgm1 :: Term
pgm1 =
  App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :+: (Con 11))

-- Ar trebui să dea eroare
pgm2 :: Term
pgm2 = App (Con 2) (Con 3)

-- test pgm
-- test pgm1
-- test pgm2
