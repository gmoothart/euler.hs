
--
-- Project Euler #5
-- 2520 is the smallest number that can be divided by each of the numbers from
-- 1 to 10 without any remainder. What is the smallest number that is evenly
-- divisible by all of the numbers from 1 to 20?
--

import Data.List
import System.CPUTime (getCPUTime)


--
-- Wow, that was easy!
--
f :: Integer -> Integer
f n  = foldl' lcm n [2..n-1]

--
-- Here's another idea: loop by 20s until we find a multiple of 19, then
-- loop by 19s until we find a multiple of 18... and so on
-- Choose a smart starting point: find all the primes in 1..20, multiply them together.
-- The answer cannot be less than that. If I do that, I think I need to work up from
-- lower -> higher instead of high -> low
-- BUT: prime testing is hard in the general case, isn't it?
--

--
-- Here's an idea from the notes: spin through the primes < k, and find their 
-- exponents. Then just multiple together to get the final number
--


f2 :: Integer -> Integer
f2 n  = foldl' g 1 (takeWhile (<= n) all_primes)
  where 
    g prod prime = let exp = floor (log (fromIntegral n) / log (fromIntegral prime))
                  in prod * (prime ^ exp)

    --primes sieve, from http://www.haskell.org/haskellwiki/Prime_numbers
    all_primes :: [Integer]
    all_primes = 2:3:primes'
      where
          1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
          primes'        = p : filter isPrime candidates
          isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
          divides n p    = n `mod` p == 0

main = do
  putStrLn "*** naive method ***"
  putStrLn (show (f 20))
  start <- getCPUTime
  putStrLn (take 10 (show (f 200000)))
  end <- getCPUTime
  print (show (end - start) ++ " ticks")

  putStrLn "*** smarter, not harder ***"
  putStrLn (show (f2 20))
  start <- getCPUTime
  putStrLn (take 10 (show (f2 200000)))
  end <- getCPUTime
  print (show (end - start) ++ " ticks")
