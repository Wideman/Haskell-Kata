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
sum2a :: Int -> Int
