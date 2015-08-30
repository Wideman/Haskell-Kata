err = error "error--99 hasskell problems"

--1 (*) Find the last element of a list.
last' :: [a] -> a
last' []     = err 
last' [x]    = x
last' (_:xs) = last' xs 


--2 (*) Find the last but one element of a list.
lastButOne :: [a] -> a
lastButOne []        = err
lastButOne [_]       = err
lastButOne xs        = xs !! (length xs - 2)


--3 (*) Find the K'th element of a list. The first element in the list is number 1
k'th :: [a] -> Int -> a
k'th xs k
    | length xs < k  = err
    | otherwise      = xs !! (k - 1)


--4 (*) Find the number of elements of a list.
length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs


--5 (*) Reverse a list.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 


--6 (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool 
isPalindrome xs = xs == reverse xs


--7 (**) Flatten a nested list structure. @attention : need recap
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x
--                 ^^^^^^^^^
--                 Map a function over a list and concatenate the results.


--8 (**) Eliminate consecutive duplicates of list elements.
elimiConsecDups :: (Eq a) => [a] -> [a] 
elimiConsecDups []     = []
elimiConsecDups [x]    = [x]
elimiConsecDups (x:xs) 
    | x == head (elimiConsecDups xs)  =  elimiConsecDups xs
    | otherwise                       =  x : elimiConsecDups xs


--9 (**) Pack consecutive duplicates of list elements into sublists. 
--       If a list contains repeated elements they should be placed 
--       in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack []      =  [[]]
pack [x]     =  [[x]]
pack (x:xs)  
    | x == (head.head $ pack xs) = (x : head (pack xs)) : tail (pack xs)
    | otherwise                  = [x] : pack xs 


--10 (*) Run-length encoding of a list. Use the result of problem P09
--       to implement the so-called run-length encoding data compression
--       method. Consecutive duplicates of elements are encoded as lists (N E)
--       where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs    =  map lamda (pack xs)
    where lambda = (\ys -> (length ys, head ys))