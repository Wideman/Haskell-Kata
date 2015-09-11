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
  | c >= '0' && c <= '9'  = ord c - ord '0'
  | c >= 'a' && c <= 'f'  = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F'  = ord c - ord 'A' + 10
  | otherwise             = error "not hexadecimal digit"

netEarning :: Float -> Float
netEarning x 
  | x < 0   = error "Input must be greater or equal to 0.00"
  | x >= 0          &&  x <= 14000.000  = x * (1 - 0.105)
  | x > 14000.000   &&  x <= 48000.000  = x * (1 - 0.175)
  | x > 48000.000   &&  x <= 70000.000  = x * (1 - 0.300)
  | x > 70000.000                       = x * (1 - 0.330)
