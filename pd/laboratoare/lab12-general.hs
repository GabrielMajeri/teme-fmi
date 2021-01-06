import Control.Monad
import Data.Maybe

--- Monada Identity
newtype Identity a = Identity {runIdentity :: a}

-- Afișează valoarea conținută în monada identitate
instance Show a => Show (Identity a) where
  show = show . runIdentity

instance Monad Identity where
  -- Creez o nouă valoare din monadă folosind direct constructorul
  return = Identity
  -- Pur și simplu aplic funcția pe valoarea conținută
  (Identity x) >>= f = f x

-- În versiunile mai noi de Haskell trebuie să definesc și instanțe ale acestor clase:
instance Functor Identity where
  fmap = liftM
instance Applicative Identity where
  pure = return
  (<*>) = ap

--- Limbajul și Interpretorul

-- Prescurtare pentru monada folosită
type M = Identity

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

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
-- Încercăm să returnăm valoarea pentru variabila dată.
-- Dacă nu există, returnăm `Wrong`
interp (Var name) env = return $ fromMaybe Wrong (lookup name env)
-- Returnăm valoarea pentru constanta dată
interp (Con i) _ = return $ Num i
-- Evalăum suma a doi termeni
interp (t1 :+: t2) env = do
  -- Evaluăm cei doi termeni
  v1 <- interp t1 env
  v2 <- interp t2 env
  -- Calculăm suma valorilor lor
  add v1 v2

interp (Lam paramName t) env = do
  -- Definim o nouă funcție, care evaluează termenul într-un environment
  -- în care avem o valoare dată pentru parametru
  let lambdaFunc = \x -> interp t ((paramName, x) : env)
  return $ Fun lambdaFunc

-- Aplicăm cei doi termeni
interp (App t1 t2) env = do
  -- Le evaluăm valorile
  v1 <- interp t1 env
  v2 <- interp t2 env
  -- Aplicăm primul termen cu al doilea
  apply v1 v2

test :: Term -> String
test t = showM $ interp t []

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
