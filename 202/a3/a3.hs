--1.a
--let lastEight x y | y >= 1 = mod(x * lastEight x (y - 1)) 100000000 | otherwise = 1
--1.b
--let numOfFact x = ceiling $ logBase 10 $ factorial (x)
