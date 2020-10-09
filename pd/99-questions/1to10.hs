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
