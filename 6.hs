
import Data.List

--
-- Project Euler #6
-- Find the difference between the sum of the squares of the first one hundred
-- natural numbers and the square of the sum.
--

sumOfSquares :: Integer -> Integer
--sumOfSquares n = foldl' (+) 0 [x*x | x <- [1..n]]
sumOfSquares n = sum zipped
  where
    -- the squares sequence increases by consecutive odd numbers each time:
    --   1, 4, 9, 16  -> 1, (1+3), (1+3+5), (1+3+5+7)
    -- I do not know why this is, but it lets us compute the sum easily. We need
    -- to add n 1s, n-1 3s, and so forth.
    zipped = zipWith f [n,n-1..1] (take (fromIntegral n) [1,3..])
    f int odd = int * odd


squareOfSums :: Integer -> Integer
squareOfSums n = let sum = floor (fromIntegral(n + n*n) / 2)
                     --sum = foldl' (+) 0 [1..n]
                 in sum * sum

main = do
  putStrLn (show (squareOfSums 100 - sumOfSquares 100))
