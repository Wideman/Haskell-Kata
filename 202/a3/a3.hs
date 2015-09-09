import Data.Char
--
--Exercise 1
--
--a
--let lastEight x y | y >= 1 = mod(x * lastEight x (y - 1)) 100000000 | otherwise = 1
--b
--let numOfFact x = ceiling $ logBase 10 $ factorial (x)
--c
--(-1,"FUNTHOMAS",False)
--d
--Prelude Data.Char> chr 100
--'d'
--Prelude Data.Char> ord 'Q'
--81

--
--Exercise 2
--
--sum2a takes O(1) whereas sum2b and sum2c take O(n), hence sum2a has best
--run time efficiency.
--
sum2a :: Int -> Int
sum2a n = (div (n * (n + 1)) 2) ^ 2

sum2b :: Int -> Int
sum2b n
  | n == 1    = 1
  | otherwise = (n ^ 3) + sum2b(n - 1)

sum2c :: Int -> Int
sum2c 1 = 1
sum2c n = (n ^ 3) + sum2b(n - 1)

--
--Exercise 3
--
f :: Char -> Int
f c
  | c >= '0' && c <= '9'  = ord c
  | c >= 'a' && c <= 'f'  =
  | c >= 'A' && c <= 'F'  =
  | otherwise     
