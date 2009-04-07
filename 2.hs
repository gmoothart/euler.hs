{- 
 - Sum the even fibonacci numbers that do not exceed 4 million
 - 
 - Is there a clever way to do this?
 -
 - -}

-- To avoid a stack overflow, use accumulators.
-- b1 and b2 are the previous fibs, sum is the running total,
-- max is the number of fibs to test, n is the current fib
fib_evens_sum_r                 :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer  
fib_evens_sum_r n b1 b2 sum max 
  | max <= 2      = error "must choose n > 2!"
  | n == max      = sum
  | otherwise     = fib_evens_sum_r (n+1) b2 (b2+b1) (if (even (b1+b2)) then sum+b1+b2 else sum ) max

fib_evens_sum max = fib_evens_sum_r 2 1 2 2 max

fibs :: [Integer]
fibs =  1 : 2 : [ a+b | (a,b) <- zip fibs (tail fibs)]

evens  :: Integer -> Integer -> Integer
evens x sum | (even x)   = x + sum
            | otherwise  = sum


main = do
  print (fib_evens_sum 10)
  print ( sum (filter (\x -> x `mod` 2 == 0) (take 10 fibs) ) )
  -- I thought this one would work...
  --print (foldr evens 0 (take 10 fibs))
  --return () >>= \_ -> print (foldr evens 0 (take 4000000 fibs))
