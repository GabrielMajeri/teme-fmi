import Control.Monad (liftM, ap)
import Data.Char (isUpper)
import Data.List (isPrefixOf)

-- Funcție care primește un string și verifică
-- dacă toate vocalele din șir sunt literă mare.
esteVocala :: Char -> Bool
esteVocala ch = ch `elem` "aeiouAEIOU"

verifVocaleMari :: String -> Bool
verifVocaleMari sir = foldr (&&) True [isUpper ch | ch <- sir, esteVocala ch]

--- Funcție care primește trei liste `x`, `y`, `z` și verifică dacă lista `z`
-- conține toate sumele de forma (x_i + y_i)
verifListe :: [Int] -> [Int] -> [Int] -> Bool
verifListe x y z =
    all (`elem` z) sume
    where sume = [a + b | a <- x, b <- y]

-- Funcție care elimină aparițiile multiple consecutive dintr-o listă de numere
elimDup :: [Int] -> [Int]
elimDup [] = []
elimDup [x] = [x]
elimDup (x:y:xs) =
    if x == y then elimDup (y:xs)
    else x:elimDup (y:xs)

-- Funcție care verifică că o listă dată este palindrom
verifPalindrom :: Eq a => [a] -> Bool
verifPalindrom [] = True
verifPalindrom [_] = True
verifPalindrom (x:xs) = (x == last xs) &&
    verifPalindrom (init xs)

-- Verifică dacă toate șirurile din listă care au ca prefix cuvântul dat
-- au lungimea mai mare decât `n`.
verifToatePrefixN :: String -> [String] -> Int -> Bool
verifToatePrefixN cuvant siruri n =
    all verifLungime siruriCuPrefix
    where
        verifLungime = (> n) . length
        siruriCuPrefix = filter (cuvant `isPrefixOf`) siruri


-- Evaluarea expersiilor cu monada State
type Name = String
data Exp = Var Name | Con Integer | Exp :/: Exp
    deriving (Show)
type Env = [(Name, Integer)]

newtype Reader env a = Reader { runReader :: env -> Maybe a }
type EnvReader = Reader Env

instance Monad (Reader env) where
    return = Reader . const . Just
    ma >>= f = Reader $ \env ->
        let
            value = runReader ma env
        in
            runReader (process value) env
        where
            process (Just x) = f x
            process Nothing = Reader (const Nothing)

instance Functor (Reader env) where
  fmap = liftM

instance Applicative (Reader env) where
    pure = return
    (<*>) = ap

ask :: Reader env env
ask = Reader $ \env -> Just env

eval :: Exp -> EnvReader Integer
eval (Var name) = do
    env <- ask
    let result = lookup name env
    Reader (const result)
eval (Con value) = do
    return $ value
eval (a :/: b) = do
    va <- eval a
    vb <- eval b
    if vb == 0 then
        Reader (const Nothing)
    else
        return $ (va `div` vb)

run :: Env -> Exp -> Maybe Integer
run env exp = runReader (eval exp) env


-- Arbori ternari cu informații doar pe frunze
-- Rezolvarea se află în fișierul `../examen/examen.hs`


-- Expresii cu doar trei variabile
data Expr = X | Y | Z
    | Plus Expr Expr
    | Ori Expr Expr

newtype Reader' a = Reader' { runReader' :: Int -> Int -> Int -> a }

instance Monad Reader' where
    return x = Reader' (\_ _ _ -> x)
    ma >>= f = Reader' $ \x y z ->
        let value = runReader' ma x y z
        in runReader' (f value) x y z

instance Functor Reader' where
  fmap = liftM

instance Applicative Reader' where
    pure = return
    (<*>) = ap

eval' :: Expr -> Reader' Int
eval' X = Reader' $ \x _ _ -> x
eval' Y = Reader' $ \_ y _ -> y
eval' Z = Reader' $ \_ _ z -> z
eval' (Plus a b) = do
    va <- eval' a
    vb <- eval' b
    return $ va + vb
eval' (Ori a b) = do
    va <- eval' a
    vb <- eval' b
    return $ va * vb

run' :: Int -> Int -> Int -> Expr -> Int
run' x y z expr = runReader' (eval' expr) x y z
