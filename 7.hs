
--
-- Project Euler #7
--  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
--  that the 6^(th) prime is 13. What is the 10001^(st) prime number?
--

--primes sieve, from http://www.haskell.org/haskellwiki/Prime_numbers
all_primes :: [Integer]
all_primes = 2:3:primes'
  where
      1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
      primes'        = p : filter isPrime candidates
      isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
      divides n p    = n `mod` p == 0

main = do
  putStrLn (show (all_primes !! 10000))
