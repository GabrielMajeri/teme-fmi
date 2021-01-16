import Control.Monad.State

type Input = String
type Output = String
type InOutWorld = (Input, Output)
type MyIOState a = State InOutWorld a

-- `MyIOState` conține o computație (o funcție) care primește la intrare
-- un obiect de tipul `InOutWorld`, și produce un obiect de tipul `a`
-- (modificând, posibil, `InOutWorld`, și returnându-i noua valoare).


-- Funcție care citește un caracter din `Input`
myGetChar :: MyIOState Char
myGetChar = do
    -- Citesc valorile curente ale stării
    (input, output) <- get

    -- Extrag primul caracter
    let (ch:restOfInput) = input

    -- Actualizez starea curentă
    put (restOfInput, output)

    -- Returnez caracterul extras
    return ch

myPutChar :: Char -> MyIOState ()
myPutChar ch = do
    -- Citesc starea curentă
    (input, output) <- get
    -- Adaug caracterul la finalul output-ului
    put (input, output ++ [ch])

runIO :: Input -> MyIOState () -> String
runIO input io =
    let
        -- Rulez computația dată pe un input dat,
        -- și pe un output buffer gol
        (finalInput, finalOutput) = execState io (input, "")
    in
        -- Afișez rezultatul
        "Input: " ++ finalInput ++ "\nOutput: " ++ finalOutput

myGetLine :: MyIOState String
myGetLine = do
    -- Citesc un caracter
    x <- myGetChar
    case x of
        -- Când am citit un newline, returnez
        '\n' -> return []
        _ -> do
            -- Apelez recursiv `myGetLine`
            rest <- myGetLine
            -- Concatenez la rezultatul final
            return $ x : rest

myPutStr :: String -> MyIOState ()
myPutStr str = do
    -- Pentru fiecare caracter din `str`, apelez `myPutChar`.
    forM_ str myPutChar


---- Teste ----
t1 = runIO "Ana\n" f
    where f = do
            l <- myGetLine
            myPutStr "Salut!\n"
            myPutStr $ "Linia introdusă este: " ++ l ++ "\n"

t2 = runIO "" f
    where f = do
            forM_ [1..10] $ \n -> do
                myPutStr $ show n
                myPutStr " "
