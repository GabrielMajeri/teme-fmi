-- Finds the last element of a list
myLast :: [a] -> a

-- For empty list, return an error
myLast [] = undefined

-- If list has one element, return it element
myLast (last:[]) = last

-- Recursively search for the last element
myLast (_:restOfTheList) = myLast restOfTheList



-- Finds the second last element of a list
myButLast :: [a] -> a

myButLast [] = undefined
myButLast [_] = undefined
myButLast [x, _] = x
myButLast (_:restOfTheList) = myButLast restOfTheList


-- Finds the k-th element of a list
elementAt :: [a] -> Int -> a

elementAt list index = list !! (index - 1)


-- Finds the number of elements in a list
myLength :: [a] -> Int

myLength list = sum $ map (\_ -> 1) list


-- Reverses a list.
myReverse :: [a] -> [a]

myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


-- Checks if a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool

isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome $ init xs)
