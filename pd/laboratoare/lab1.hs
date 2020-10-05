-- Funcție care calculează suma pătratelor a două numere
sumaPatrate :: Integer -> Integer -> Integer
sumaPatrate x y = x^2 + y^2

-- Returnează un string care indică dacă parametrul este par
estePar :: Integer -> String
estePar n = if n `mod` 2 == 0 then "par" else "impar"

-- Calculează factorialul unui număr
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Verifică dacă primul parametru este mai mare decât dublul celui de al doilea parametru
verifDublu :: Integer -> Integer -> Bool
verifDublu a b = a > dublu
    where dublu = 2 * b
