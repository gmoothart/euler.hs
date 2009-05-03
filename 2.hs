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
-- and the golden-ratio method. Many thanks to:
-- http://github.com/raganwald/homoiconic/blob/master/2008-12-12/fibonacci.md
-- http://expertvoices.nsdl.org/cornell-cs322/2008/03/25/sum-of-fibonacci-numbers/200/
-- http://www.johndcook.com/blog/2008/04/23/fibonacci-numbers-at-work/
--
-- Is there a matrix for the even fibonacci numbers? Where would the (4*) fit?
--   No, there isn't a straightforward matrix-multiplication answer. BUT, we can
--   define our own operator which behaves correctly! I think?
--
--------------------------------------------------------------------------------

type Fiblist = (Integer, Integer, Integer)

i = (1,0,1)
b = (8,2,0)
m = (4,1,0)

first           :: Fiblist -> Integer
first (a, b, c) = a

(.+)                           :: Fiblist -> Fiblist -> Fiblist
(.+) (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)

(.*)                           :: Fiblist -> Fiblist -> Fiblist
(.*) (a1, a2, a3) (b1, b2, b3) = (a1*b1 + a2*b2, a1*b2 + a2*b3, a2*b2 + a3*b3)

--this can be faster...
(.^) :: (Integral a) => Fiblist -> a -> Fiblist
(.^) (a, b, c) exp 
  | exp == 1    = (a, b, c)
  | otherwise   = (a, b, c) .* ((a, b, c) .^ (exp-1))


-- top-level, bootstrapping function
fibSum        :: (Integral a) => a -> Integer
fibSum 0      = 0
fibSum 1      = 2
fibSum n      = first (b .* (fibSum' (n-2)))

--recursive function. Does the heavy lifting
--  this does what I think it should... but doesn't work!
fibSum'       :: (Integral a) => a -> Fiblist
fibSum' n
  | n == 0    = i
  | odd n     = let s = fibSum' ( (n-1) `div` 2 )
                    t = m .^ ( (n+1) `div` 2 ) --this can be optimized
                in (t .* s) .+ s --this can be optimized
  | even n    = let s = fibSum' ((n `div` 2) - 1)
                    t = m .^ (n `div` 2)
                in (t .* s) .+ s .+ (m .^ n)


main = do
  putStrLn "*** naive method ***"
  print (take 20 fibs)
  print (foldr evens 0 (takeWhile (<= top) fibs))
  print (fib_evens_sum 10)
  print ( sum (filter (\x -> x `mod` 2 == 0) (takeWhile (<= top) fibs) ) )
  putStrLn "*** Smarter ***"
  print (take 10 fibs2)
  print (sum (takeWhile (<=top) fibs2))
