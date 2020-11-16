-- Implementarea microHaskell
import Data.Maybe

type Name = String

data Value
  = VBool Bool
  | VInt Int
  | VFun (Value -> Value)
  | VError

data Hask
  = HTrue
  | HFalse
  | HIf Hask Hask Hask
  | HLit Int
  | Hask :==: Hask
  | Hask :+: Hask
  | Hask :*: Hask
  | HVar Name
  | HLam Name Hask
  | HLet Name Hask Hask
  | Hask :$: Hask
  deriving (Read, Show)

infix 4 :==:

infixl 6 :+:
infixl 7 :*:

infixl 9 :$:

type HEnv = [(Name, Value)]

showV :: Value -> String
showV (VBool b) = show b
showV (VInt i) = show i
showV (VFun _) = "<function>"
showV VError = "<error>"

eqV :: Value -> Value -> Bool
eqV (VBool b) (VBool c) = b == c
eqV (VInt i) (VInt j) = i == j
eqV (VFun _) (VFun _) = error "Cannot compare functions for equality"
eqV VError VError = error "Cannot compare errors for equality"
eqV _ _ = False

hEval :: Hask -> HEnv -> Value
hEval HTrue _ = VBool True
hEval HFalse _ = VBool False
hEval (HIf c d e) r =
  hif (hEval c r) (hEval d r) (hEval e r)
  where
    hif (VBool b) v w = if b then v else w
    hif _ _ _ = error "If condition must be a boolean expression"
hEval (HLit i) _ = VInt i
hEval (d :==: e) r = heq (hEval d r) (hEval e r)
  where
    heq (VInt i) (VInt j) = VBool (i == j)
    heq _ _ = error "Only integers can be compared for equality"
hEval (d :+: e) r = hadd (hEval d r) (hEval e r)
  where
    hadd (VInt i) (VInt j) = VInt (i + j)
    hadd _ _ = error "Only integers can be added"
hEval (d :*: e) r = hmul (hEval d r) (hEval e r)
  where
    hmul (VInt i) (VInt j) = VInt (i * j)
    hmul _ _ = error "Only integers can be multiplied"
hEval (HVar x) r = fromMaybe (error "Variable not found") (lookup x r)
hEval (HLam x e) r = VFun (\v -> hEval e ((x, v) : r))
hEval (HLet name value expression) env =
    let
        -- Determin valoarea variabilei nou introduse
        v = hEval value env
        -- O adaug la environment-ul în care evaluez expresia din interior
        newEnv = env ++ [(name, v)]
    in
        hEval expression newEnv
hEval (d :$: e) r = happ (hEval d r) (hEval e r)
  where
    happ (VFun f) v = f v
    happ _ _ = error "Cannot apply non-function to a value"

run :: Hask -> String
run pg = showV (hEval pg [])

h0 =
  HLam "x" (HLam "y" (HVar "x" :+: HVar "y"))
    :$: HLit 3
    :$: HLit 4

h1 =
    HLit 1 :+: HLit 2 :+: HLit 3

h2 =
    HLam "x" (HLit 1 :+: HVar "x")

h3 = h2 :$: h1

h4 = HLit 3 :*: HLit 2

h5 = HLet "x" (HLit 3) (HLit 4 :+: HVar "x")

test_h0 = eqV (hEval h0 []) (VInt 7)
