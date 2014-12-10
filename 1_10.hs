{-|
 - Problem 1
 - Find the last element of a list.
 -
 - ? this way causes a linear complexity.Is there a better way
 -}
last' :: [a] -> a
last' [] = error "empty list\n"
last' [x] = x
last' (_:xs)    =   last' xs


{-|
 - Problem 2
 - Find the last but one element of a list.
 -}
the_second_last::[a]->a
the_second_last []      =   error "too short"
the_second_last [_]     =   error "too short"
the_second_last (x:xs)
    |   length xs == 1  =   x
    |   otherwise       =   the_second_last xs


{-
 - Problem 3
 - Find the K'th element of a list. The first element in the list is number 1.
 -}
element_at :: [a] -> Int -> a
element_at li i         =   li !! (i-1)


{- Problem 4
 - Find the number of elements of a list.
 - -}
len' :: [a] -> Int 
len' []         =   0
len' (x:xs)     =   1 + len' xs


{- Problem 5
 - Reverse a list.
 - -}
reverse' :: [a] -> [a]
reverse' []     =   []
--reverse' [x]    =   [x]   -- unnecessary
reverse' (x:xs) =   reverse' xs ++ [x]


{- Problem 6
 - Find out whether a list is a palindrome. 
 - -}
is_palindrome :: (Eq a) =>  [a] -> Bool
is_palindrome li =   li == reverse' li    


{- Problem 7
 - Flatten a nested list structure.
 - --copied from haskell.org
 - -- need check info for type def
 - -}
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a)    =   [a]
flatten (List(x:xs))=   flatten x ++ flatten (List xs)
flatten (List[])    =   []

{- Problem 8
 - Eliminate consecutive duplicates of list elements.
 - -}
compress (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress x = x








