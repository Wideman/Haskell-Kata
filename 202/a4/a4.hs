--
-- Exercise 1
-- a
time  :: Integer -> (Integer, Integer, Integer)
time i = ((div i 3600), (div (mod i 3600) 60), (mod i 60))

--b

--
-- Exercise 2
--a
type Fraction = (Int, Int)

showFrac :: Fraction -> String
showFrac frac = show (fst frac) ++ "/" ++ show (snd frac) ++ "\n"

makeFrac :: Fraction -> Fraction
makeFrac frac = (first, second)
  where
    first   =   div (fst frac) common
    second  =   div (snd frac) common
    common  =   gcd (fst frac) (snd frac)

powFr :: Fraction -> Int -> String
powFr b p = showFrac (makeFrac calculated)
  where calculated = ((fst b) ^ p , ((snd b) ^ p))
--b
--not implemented yet.
--
-- Exercise 3
--
--a
prod :: [Integer] -> [Integer] -> Integer
prod [_] []        = error "Error: different sizes."
prod [] [_]        = error "Error: different sizes."
prod []  []        = 1
prod (l:ls) (r:rs) = (check l r) * (prod ls rs)
  where check i j = if mod i j == 0 then i else 1

--b
smallest :: [Int] -> (Int, Int)
smallest []     = error "empty list"
smallest [x]    = (0, x)
smallest (x:xs) = if x < (snd next) then (0, x) else ((fst next) + 1, snd next )
  where next = smallest xs

magic :: [Int] -> (Int, [Int])
magic xs = (snd pair, lft ++ rht)
  where
    lft = take (fst pair) xs
    rht = drop ((fst pair) + 1) xs
    pair = smallest xs

--c
total :: [[Int]] -> Int
total []      = 0
total (x:xs)  = (sum x) + (total xs)

--
--Exercise 4
--
sOdd1 :: [Int] -> [Int]
sOdd1 []      = []
sOdd1 [_]     = []
sOdd1 (x:xs)  = (if odd x then [head xs] else [] ) ++ sOdd1 xs

sOdd2 :: [Int] -> [Int]
sOdd2 xs
  | length xs > 1  =  (if odd (head xs) then [xs !! 1] else []) ++ sOdd2 (tail xs)
  | otherwise      =  []

sOdd3 :: [Int] -> [Int]
sOdd3 xs = [ xs !! (i + 1) | i <- [0..((length xs) - 2)], odd (xs !! i) ]
