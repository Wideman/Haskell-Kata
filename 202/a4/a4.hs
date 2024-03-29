--
-- Exercise 1
-- a
time  :: Integer -> (Integer, Integer, Integer)
time i = (div i 3600, div (mod i 3600) 60, mod i 60)

--b
type Point = (Int, Int)
type Line = (Int, Int, Int)
onLine :: Point -> Line -> Bool
onLine p l =  0 == a * x + b * y + c
  where a = item0 l
        b = item1 l
        c = item2 l
        x = fst p
        y = snd p
        item0 (a, _, _) = a
        item1 (_, a, _) = a
        item2 (_, _, a) = a
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
(%%) :: Fraction -> Fraction -> Bool
l %% r = l == (snd r, fst r)
infix 3 %%

--
-- Exercise 3
--
--a
prod :: [Integer] -> [Integer] -> Integer
prod [_] []        = error "Error: different sizes."
prod [] [_]        = error "Error: different sizes."
prod []  []        = 1
prod (l:ls) (r:rs) 
  | mod l r == 0   = l * next
  | otherwise	   = 1 * next
  where next = prod ls rs

--b
smallest :: [Int] -> (Int, Int)
smallest []        = error "empty list"
smallest [x]       = (0, x)
smallest (x:xs)
  | x < (snd next) = (0, x) 
  | otherwise      = ( (fst next) + 1, snd next )
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
total (x:xs)  = sum x + total xs

--
--Exercise 4
--
sOdd1 :: [Int] -> [Int]
sOdd1 []      = []
sOdd1 [_]     = []
sOdd1 (x:xs)  = curr ++ sOdd1 xs
  where curr = if odd x then [head xs] else []

sOdd2 :: [Int] -> [Int]
sOdd2 xs
  | length xs > 1  = curr ++ sOdd2 (tail xs)
  | otherwise      =  []
  where curr = if odd (head xs) then [xs !! 1] else []

sOdd3 :: [Int] -> [Int]
sOdd3 xs = [ xs !! (i + 1) | i <- [0..((length xs) - 2)], odd (xs !! i) ]
