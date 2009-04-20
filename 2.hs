{- 
 - Sum the even fibonacci numbers that do not exceed 4 million
 - 
 - Is there a clever way to do this?
 -
 - -}

top = 4000000

-------------------------------------------------------------------------------
-- Naive calculation
-------------------------------------------------------------------------------

-- Using accumulators and explicit recursion
-- b1 and b2 are the previous fibs, sum is the running total,
-- max is the number of fibs to test, n is the current fib
fib_evens_sum_r                 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer  
fib_evens_sum_r n b1 b2 sum max 
  | max <= 2      = error "must choose n > 2!"
  | b1+b2 > top   = sum
  | otherwise     = fib_evens_sum_r (n+1) b2 (b2+b1) (if (even (b1+b2)) then sum+b1+b2 else sum ) max

fib_evens_sum max = fib_evens_sum_r 2 1 2 2 max

-- infinite list
fibs :: [Integer]
fibs =  1 : 2 : [ a+b | (a,b) <- zip fibs (tail fibs)]

evens  :: Integer -> Integer -> Integer
evens x sum | (even x)   = x + sum
            | otherwise  = sum

--------------------------------------------------------------------------------
-- Smarter: every 3rb fib is even, and there is a relationship between every
--          3rd fib:
--          f(n) = 4*f(n-3) + f(n-6)
--------------------------------------------------------------------------------
fibs2 :: [Integer]
fibs2 =  2 : 8 : [ a + 4*b | (a,b) <- zip fibs2 (tail fibs2)]



main = do
  putStrLn "*** naive method ***"
  print (take 20 fibs)
  print (foldr evens 0 (takeWhile (<= top) fibs))
  print (fib_evens_sum 10)
  print ( sum (filter (\x -> x `mod` 2 == 0) (takeWhile (<= top) fibs) ) )
  putStrLn "*** Smarter ***"
  print (take 10 fibs2)
  print (sum (takeWhile (<=top) fibs2))
