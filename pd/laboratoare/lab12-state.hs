import Control.Monad
import Data.Maybe

--- Monada IntState
newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }

-- Afișează valoarea conținută în monadă
instance Show a => Show (IntState a) where
  show state =
      let (value, steps) = runIntState state 0
      in "Value: " ++ show value ++ "; Count: " ++ show steps

instance Monad IntState where
  -- Creez o nouă valoare din monadă folosind direct constructorul
  return value = IntState $ \state -> (value, state)
  -- Aplic două transformări de stare, una după alta
  (IntState ma) >>= k = IntState $ \state ->
      let
        (value, newState) = ma state
        (IntState f) = k value
        (newValue, finalState) = f newState
      in
        (newValue, finalState)

-- În versiunile mai noi de Haskell trebuie să definesc și instanțe ale acestor clase:
instance Functor IntState where
  fmap = liftM
instance Applicative IntState where
  pure = return
  (<*>) = ap

--- Limbajul și Interpretorul

-- Prescurtare pentru monada folosită
type M = IntState

showM :: Show a => M a -> String
showM = show

-- Aplică funcția dată pe starea curentă, modificând-o
modify :: (Integer -> Integer) -> IntState ()
modify f = IntState $ \state -> ((), f state)

-- Crește cu 1 starea curentă
tickS :: IntState ()
tickS = do
  modify (+1)

-- Returnează valoarea curentă a numărului de pași
get :: IntState Integer
get = IntState $ \state -> (state, state)

type Name = String

data Term
  = Var Name
  | Con Integer
  | Count
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

-- Returnăm valoarea curentă a numărului de pași
interp Count env = IntState $ \state -> (Num state, state)

-- Evaluăm suma a doi termeni
interp (t1 :+: t2) env = do
  -- Evaluăm cei doi termeni
  v1 <- interp t1 env
  v2 <- interp t2 env
  -- Creștem numărul de pași efectuați
  tickS
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
  -- Creștem numărul de pași efectuați
  tickS
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

-- Ar trebui să afișeze "Value: 4; Count: 2"
pgm2 :: Term
pgm2 = (Con 1 :+: Con 2) :+: Count

-- test pgm
-- test pgm1
-- test pgm2
