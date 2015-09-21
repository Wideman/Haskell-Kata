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
showFrac frac = "(" ++ show (fst frac) ++ "/" ++ show (snd frac) ++ ")"

makeFrac :: Fraction -> Fraction
makeFrac frac = (first, second)
  where 
    first   =   div (fst frac) common
    second  =   div (snd frac) common
    common  =   gcd (fst frac) (snd frac)

powFr :: Fraction -> Int -> String
powFr b p = showFrac (makeFrac calculated)
  where calculated = ((fst b) ^ p , ((snd b) ^ p))
--
-- Exercise 3
--

