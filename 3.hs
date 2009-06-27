
-- Project Euler #3
--What is the largest prime factor of the number 600851475143 ?


n = 600851475143

--primes sieve, from http://www.haskell.org/haskellwiki/Prime_numbers
all_primes :: [Integer]
all_primes = 2:3:primes'
  where
      1:p:candidates = [6*k+r | k <- [0..], r <- [1,5]]
      primes'        = p : filter isPrime candidates
      isPrime n      = all (not . divides n) $ takeWhile (\p -> p*p <= n) primes'
      divides n p    = n `mod` p == 0


-- could we re-write this using fold?
lpf :: Integer -> [Integer] -> Integer
lpf n primes = lpf' n primes 0
  where lpf' :: Integer -> [Integer] -> Integer -> Integer
        lpf' n primes max_so_far
          | p == n             = p
          | p > n              = max_so_far
          | n `mod` p == 0     = lpf' (n `div` p) (tail primes) p
          | otherwise          = lpf' n (tail primes) max_so_far
          where p = (head primes)

main = do
  print (lpf n all_primes)

