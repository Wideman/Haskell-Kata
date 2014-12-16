err = error "errors ..."

--Problem 21
--Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt elem [] _ = [elem]
insertAt elem xs 0 = elem : xs
insertAt elem (x:xs) n = (:) x $ insertAt elem xs $ n - 1
