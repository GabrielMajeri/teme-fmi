-- Function definition
square :: Num a => a -> a
square x = x * x

{-
 Multiline
 comment
-}

-- Custom operator
infixr 5 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + 2 * b

intConstant :: Integer
intConstant = 42

floatConstant = 3.14


type Name = String

data Simple = S Int Int deriving Show

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)


filterNumbers :: [Integer] -> [Integer]
filterNumbers = filter squareIsOdd
    where squareIsOdd = odd . (^2)
