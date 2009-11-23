
-- Project Euler #4:
-- Find the largest palindromic number divisible by two 3-digit numbers

import List

isPalindrome :: Integer -> Bool
isPalindrome x = isPalindromeString (show x)
  where isPalindromeString :: [Char] -> Bool
        isPalindromeString []   = True
        isPalindromeString (a:[]) = True
        isPalindromeString s    = (head s) == (last s) && (isPalindromeString inner)
          where inner = tail (init s)

isDivisible :: Integer -> Bool
isDivisible x = any (\y-> x `mod` y == 0) [999, 998..(ceiling (sqrt (fromIntegral x)))]

f :: Integer -> Integer
f x 
  | x < 100*100      = -1 --no solution
  | isPalindrome x   = if isDivisible x
                         then x
                         else f x
  | otherwise        = f (x-1)

main = do
  putStrLn (show (find (\x -> isPalindrome x && isDivisible x) [999*999, (999*999 - 1)..100*100]))
