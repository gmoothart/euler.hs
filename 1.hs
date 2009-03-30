--
-- Euler project #1
-- 

import List

--helper methods
div_3 x = (mod x 3) == 0
div_5 x = (mod x 5) == 0

sum_first_1k x = sum (takeWhile (< 1000) x)

--  sum multiples of 3, and add multiples of 5 that are not also
--  multiples of 3
threes1 = [x*3 | x <- [1..]]
fives1  = [x*5 | x <- [1..], (div_3 (x*5)) == False]


--using addition instead of multiplication
threes2 = 3 : [ x+3 | x <- threes2]
fives2 = 5 : [ if div_3 (x+5) then x+10 else x+5 | x <- fives2 ]

-- same as above, but via map and pattern-matching
next x | div_3 (x+5)   = x+10 -- if x is a mutiple of 3, go to the next one
       | otherwise     = x+5
fives2b  = 5 : [ x | x <- map next fives2b]

-- using the iterate function instead of list comprehensions
threes3 = (iterate (+3) 3)
fives3  = (iterate (+5) 5)
both3   = (iterate (+15) 15)

--Just for fun:
fibs = 0 : 1 : [a + b| (a, b) <- zip fibs (tail fibs)]

main = do
         --brute-force
         --  check every number from 3 to 1000
         print (sum (filter (\x -> (div_3 x) || (div_5 x)) [3..999]))
         --smarter: generate list by multiplication
         --  i.e., 1,2,3 -> 3,6,9
         print ((sum_first_1k threes1) + (sum_first_1k fives1))
         --using addition
         print ((sum_first_1k threes2) + (sum_first_1k fives2))
         print ((sum_first_1k threes2) + (sum_first_1k fives2b))
         --using the iterate function
         print ((sum_first_1k threes3) + sum ((takeWhile (<1000) fives3) \\ (takeWhile (<1000) both3)))
         --just for fun:
         print (take 10 fibs)



