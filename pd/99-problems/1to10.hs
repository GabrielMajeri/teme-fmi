---- Problem 1
-- Finds the last element of a list
myLast :: [a] -> a

-- For empty list, return an error
myLast [] = undefined

-- If list has one element, return it
myLast [last] = last

-- Recursively search for the last element
myLast (_:restOfTheList) = myLast restOfTheList


---- Problem 2
-- Finds the second last element of a list
myButLast :: [a] -> a

myButLast [] = undefined
myButLast [_] = undefined
myButLast [x, _] = x
myButLast (_:restOfTheList) = myButLast restOfTheList


---- Problem 3
-- Finds the k-th element of a list
elementAt :: [a] -> Int -> a

elementAt list index = list !! (index - 1)


---- Problem 4
-- Finds the number of elements in a list
myLength :: [a] -> Int

myLength = foldr (\_ acc -> acc + 1) 0


---- Problem 5
-- Reverses a list.
myReverse :: [a] -> [a]

myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


---- Problem 6
-- Checks if a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool

isPalindrome l = l == reverse l


---- Problem 7
data NestedList a = Elem a | List [NestedList a]
    deriving Show

flatten :: NestedList a -> [a]
flatten (Elem el) = [el]
flatten (List nested) = concatMap flatten nested


---- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [e] = [e]
compress (x:xs) = x : compress (dropWhile (== x) xs)

---- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack l@(x:xs) = takeWhile (== x) l : pack (dropWhile (== x) xs)


---- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map encodePackage . pack
    where encodePackage l = (length l, head l)
