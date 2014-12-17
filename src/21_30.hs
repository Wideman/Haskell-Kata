err = error "errors ..."

--Problem 21
--Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt elem [] _ = [elem]
insertAt elem xs 0 = elem : xs
insertAt elem (x:xs) n = (:) x $ insertAt elem xs $ n - 1


--2 Problem 22
--Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range beg end 
    | beg == end  = []
    | otherwise   = beg : range (beg + 1) end 


--Problem 28
--Sorting a list of lists according to length of sublists
import List
import Data.Ord (comparing)
lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))
