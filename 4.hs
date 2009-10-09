
-- Project Euler #4:
-- Find the largest palindromic number divisible by two 3-digit numbers

isPalindrome :: Integer -> Boolean
isPalindrome x = isPalindromeString ()
  where isPalindromeString :: [Char] -> Boolean
        isPalindromeString []   = True
        isPalindromeString a:[] = True
        isPalindromeString s    = (first s) == (last s) && (isPalindrome inner)
          where inner = rest (init s)

isDivisible :: Integer -> Boolean
isDivisible x = any [999...(sqrt x)] \y-> x `mod` y == 0

f :: Integer -> Integer
f x = 
  | x < 100*100      = -1 --no solution
  | isPalindrome x   = if isDivisible x
                         then x
                         else f x
  | otherwise        = f (x-1)

any (\x -> isPalindrome x && isDivisible x) [999*999..100*100] 
