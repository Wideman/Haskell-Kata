--14 (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs 


--15 (**) Replicate the elements of a list a given number of times.
repli :: [a] ->  Int -> [a]
repli [] _ = []
repli (x:xs) 0 = []
repli (x:xs) n = replicate n x ++ repli xs n   


--16 (**) Drop every N'th element from a list.
dropEvery :: [a] -> Integer -> [a]
dropEvery [] _ = []
dropEvery xs n = dr xs n 0
    where dr []     _ _     =  []
    	  dr (x:xs) n curr 
            |  n == curr    =  [] ++ dr xs n 0
            |  otherwise    =  x  :  dr xs n (curr + 1) 


--17 (*) Split a list into two parts; the length of the first part is given.
splitAt' :: [a] -> Int -> ([a],[a])
splitAt' xs n = (take n xs, drop n xs)


--18 (**) Extract a slice from a list.
slice' :: [a] -> Int -> Int -> [a]
slice' xs n k = drop (n-1) $ take k xs 


--19 (**) Rotate a list N places to the left.
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n ys@(x:xs) 
    | n > 0     = rotate (n-1) $ xs ++ [x] 
    | otherwise = rotate (abs n - 1) $ last ys : init ys
