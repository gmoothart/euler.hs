
--
-- Project Euler #5
-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder. What is the smallest number that is evenly
-- divisible by all of the numbers from 1 to 20?
--
-- Wow, that was easy!
--

f :: Integer
f  = foldl lcm 20 [1..19]

main = do
  putStrLn (show f)



