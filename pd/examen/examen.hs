-- Definiția tipului arbore
data Arb = Leaf Integer | Node Arb Arb Arb

-- O direcție pe care o pot lua când parcurg arborele
data Direction = L | M | R
  deriving (Eq, Show)

-- O listă de direcții care duc la o frunză
type LeafCode = [Direction]

-- Monada Writer
data Writer a = Writer
  { output :: [LeafCode],
    value :: a
  }

-- Derivez clasa de tipuri
instance Monad Writer where
    return x = Writer { output = [], value = x }
    w >>= k =
        let
            Writer out1 v1 = w
            Writer out2 v2 = k v1
        in
            Writer (out1 ++ out2) v2

instance Functor Writer where
  fmap f w = do f <$> w

instance Applicative Writer where
  pure = return
  fw <*> w = do
      f <- fw
      f <$> w

-- Funcție ajutătoare care scrie un drum în output
write :: LeafCode -> Writer ()
write code = Writer [code] ()

-- Funcție ajutătoare care primește drumul curent
leafCodes' :: Arb -> Integer -> LeafCode -> Writer ()
-- Cazul frunză: verific dacă frunza are valoarea cerută,
-- și o afișez.
leafCodes' (Leaf v) i code =
    if v == i then
        write code
    else
        return ()
-- Cazul nod: verific recursiv fiecare sub-arbore.
leafCodes' (Node left middle right) value code = do
    leafCodes' left value (code ++ [L])
    leafCodes' middle value (code ++ [M])
    leafCodes' right value (code ++ [R])

leafCodes :: Arb -> Integer -> Writer ()
leafCodes arb value = leafCodes' arb value []

---- Teste ----
-- Exemplul din cerință
tree :: Arb
tree = Node
    (Node (Leaf 1) (Leaf 3) (Leaf 4))
    (Node (Leaf 1) (Leaf 2) (Leaf 4))
    (Leaf 4)
t1 :: Bool
t1 = output (leafCodes tree 4) == [[L, R], [M, R], [R]]

-- Când nu găsește nimic, trebuie să returneze lista vidă
t2 :: Bool
t2 = output (leafCodes tree (-1)) == []

-- Când îi dau doar o frunză și aceasta se potrivește,
-- returnează o listă care conține lista vidă
-- (deci un singur drum, de lungime 0).
t3 :: Bool
t3 = output (leafCodes (Leaf 1) 1) == [[]]

-- Când îi dau doar o frunză și aceasta nu se potrivește,
-- returnează lista vidă (nu există drumuri).
t4 :: Bool
t4 = output (leafCodes (Leaf 1) 2) == []
