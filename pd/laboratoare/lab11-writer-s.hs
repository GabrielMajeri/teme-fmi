--- Monada Writer care scrie mesaje de logging într-un string
newtype WriterS a = Writer {runWriter :: (a, String)}

instance Monad WriterS where
  return va = Writer (va, "")
  ma >>= k =
    let (va, log1) = runWriter ma
        (vb, log2) = runWriter (k va)
     in Writer (vb, log1 ++ log2)

instance Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor WriterS where
  fmap f ma = pure f <*> ma

tell :: String -> WriterS ()
tell log = Writer ((), log)

---- Exercițiul 1.1 ----
-- Incrementează argumentul cu 1 și concatenează un mesaj care să anunțe asta
logIncrement :: Int -> WriterS Int
logIncrement x = Writer (x + 1, "Incrementez " ++ show x ++ "\n")

-- Apelează de două ori consecutiv funcția de incrementare de mai sus
logIncrement2 :: Int -> WriterS Int
logIncrement2 x = logIncrement x >>= logIncrement

---- Exercițiul 1.2 ----
-- Apelează de N ori funcția de incrementare
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n
    | n <= 0 = undefined
    | n == 1 = logIncrement x
    | otherwise = logIncrementN x (n - 1) >>= logIncrement
