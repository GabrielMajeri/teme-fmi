--- Monada Writer care adaugă mesaje de logging într-o listă
newtype WriterLS a = Writer {runWriter :: (a, [String])}

instance Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k =
    let (va, log1) = runWriter ma
        (vb, log2) = runWriter (k va)
     in Writer (vb, log1 ++ log2)

instance Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor WriterLS where
  fmap f ma = pure f <*> ma

tell :: String -> WriterLS ()
tell log = Writer ((), [log])

---- Exercițiul 2 ----
logIncrement :: Int -> WriterLS Int
logIncrement x = Writer (x + 1, ["Incrementez " ++ show x ++ "\n"])

logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x n
    | n <= 0 = undefined
    | n == 1 = logIncrement x
    | otherwise  = logIncrementN x (n - 1) >>= logIncrement


---- Exercițiul 5 ----
isPos :: Int -> WriterLS Bool
isPos x = if (x >= 0) then (Writer (True, ["poz"])) else (Writer (False, ["neg"]))

mapWriterLS :: (a -> WriterLS b) -> [a] -> WriterLS [b]
mapWriterLS f [] = return []
mapWriterLS f (x:xs) = do
    -- Apelez f pe primul element din listă
    fx <- f x
    -- Aplic recursiv funcția
    fxs <- mapWriterLS f xs
    -- Reunesc rezultatul
    return $ fx : fxs
