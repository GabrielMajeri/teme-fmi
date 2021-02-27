square :: Num a => a -> a
square x = x * x


intConstant = 42

floatConstant = 3.14


type Name = String

data Simple = S Int Int deriving Show

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show, Eq)
