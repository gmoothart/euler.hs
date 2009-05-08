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

--------------------------------------------------------------------------------
--
-- Smarter still: investigating the method based on matrix multiplication
-- http://github.com/raganwald/homoiconic/blob/master/2008-12-12/fibonacci.md
-- http://expertvoices.nsdl.org/cornell-cs322/2008/03/25/sum-of-fibonacci-numbers/200/
-- http://www.johndcook.com/blog/2008/04/23/fibonacci-numbers-at-work/
--
-- TODO:
-- 2) find out how to profile the implementations, for speed
-- 4) write-up as a blog post
--------------------------------------------------------------------------------

type Fiblist = (Integer, Integer, Integer)


-- This data type wraps the double-return value needed by fibSum'
data PowerSum = PowerSum {
  curr_pow :: Fiblist,
  curr_sum :: Fiblist
}

i = (1,0,1)
b = (8,2,0)
m = (4,1,0)

first           :: Fiblist -> Integer
first (a, b, c) = a

(.+)                           :: Fiblist -> Fiblist -> Fiblist
(.+) (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)

(.*)                           :: Fiblist -> Fiblist -> Fiblist
(.*) (a1, a2, a3) (b1, b2, b3) = (a1*b1 + a2*b2, a1*b2 + a2*b3, a2*b2 + a3*b3)

-- top-level, bootstrapping function
fibSum        :: (Integral a) => a -> Integer
fibSum 0      = 0
fibSum 1      = 2
fibSum n      = let sum_list = curr_sum (fibSum' (n-2))
                in  first (b .* sum_list) + 2 

--recursive function. Does the heavy lifting
-- For performance reasons, the fibSum' function must return both 
-- the sum up to n and m^n. Although this complicates bookkeeping and
-- readability, it prevents wasting a lot of time calculating the same 
-- powers of n repeatedly.
fibSum'       :: (Integral a) => a -> PowerSum
fibSum' n
  | n == 0    = PowerSum i i
  | odd n     = let ps = fibSum' (floor (fromIntegral n/2))
                    s = (curr_sum ps) -- sum as of floor(n/2) 
                    p = (curr_pow ps) -- m ^ floor(n/2)
                    t = p .* m        -- m ^ ceil(n/2)
                    m_exp_n = t .* p  -- m ^ n
                in PowerSum m_exp_n  ((t .* s) .+ s)
  | even n    = let ps = fibSum' ((n `div` 2) - 1)
                    s = (curr_sum ps) -- sum as of n/2 - 1
                    p = (curr_pow ps) -- m ^ (n/2 - 1)
                    t = p .* m        -- m ^ n/2
                    m_exp_n = t .* t  -- m ^ n
                in PowerSum m_exp_n ((t .* s) .+ s .+ m_exp_n)


main = do
  putStrLn "*** naive method ***"
  print (take 20 fibs)
  print (foldr evens 0 (takeWhile (<= top) fibs))
  print (fib_evens_sum 10)
  print ( sum (filter (\x -> x `mod` 2 == 0) (takeWhile (<= top) fibs) ) )
  putStrLn "*** Smarter ***"
  print (take 10 fibs2)
  print (sum (takeWhile (<=top) fibs2))
  putStrLn "*** Genius ***"
  print (fibSum 11)
