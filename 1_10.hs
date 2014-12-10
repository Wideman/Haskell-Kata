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




















