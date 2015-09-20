--
-- Exercise 1
--
time  :: Integer -> (Integer, Integer, Integer)
time i = ((div i 3600), (div (mod i 3600) 60), (mod i 60))
