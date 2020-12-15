import Control.Monad ((<=<))

type IntToMaybe = Int -> Maybe Int

---- Exercițiul 1.2 ----
asoc :: IntToMaybe -> IntToMaybe -> IntToMaybe -> Int -> Bool
asoc f g h x =
    (h <=< (g <=< f) $ x) == ((h <=< g) <=< f $ x)

f1 :: IntToMaybe
f1 x = if even x then Just (x `div` 2) else Nothing

f2 :: IntToMaybe
f2 x = if odd x then Just ((x * 3) `div` 4) else Nothing

f3 :: IntToMaybe
f3 x = if x > 0 then Just (-x) else Nothing

-- Funcție pentru testarea cu QuickCheck
testAsoc :: Int -> Bool
testAsoc = asoc f1 f2 f3


---- Exercițiul 2.2 ----
pos :: Int -> Bool
pos x = x >= 0

foo :: Maybe Int -> Maybe Bool
foo mx = do
    x <- mx
    return $ pos x


---- Exercițiul 3 ----
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = mx >>= (\x -> my >>= (\y -> Just $ x + y))

addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do
    x <- mx
    y <- my
    return $ x + y

testAddM :: Maybe Int -> Maybe Int -> Bool
testAddM mx my = addM mx my == addM' mx my


---- Exercițiul 4 ----
cartesianProduct :: Maybe Int -> Maybe Int -> Maybe (Int, Int)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    return (x, y)

prod :: (Int -> Int -> Int) -> [Maybe Int] -> [Maybe Int] -> [Maybe Int]
prod f xs ys = [
    do
        x <- mx
        y <- my
        return $ f x y
    | mx <- xs, my <- ys]

myGetLine :: IO String
myGetLine = do
    x <- getChar
    if x == '\n' then
        return []
    else do
        xs <- myGetLine
        return (x:xs)


---- Exercițiul 5 ----
prelNo :: Float -> Float
prelNo = sqrt

ioNumber :: IO ()
ioNumber =
    (readLn :: IO Float) >>= \noin ->
        putStrLn ("Intrare\n" ++ show noin) >>
        let
            noout = prelNo noin
        in
            putStrLn "Iesire" >>
            print noout
