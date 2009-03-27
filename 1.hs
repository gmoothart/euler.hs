--
-- Euler project #1
-- The syntax is incorrect... but I know what I want to do!

--helper methods
div_3 x = (mod x 3) == 0
div_5 x = (mod x 5) == 0

sum_first_1k x = sum (takeWhile (<= 1000) x)

--  sum multiples of 3, and add multiples of 5 that are not also
--  multiples of 3
threes1 = [x*3 | x <- [1..], div_3 x]
fives1  = [x*5 | x <- [1..], (div_5 x) == True && (div_3 x) == False]


--using addition instead of multiplication
threes2 = 3 : [ x+3 | x <- threes2]
fives2  = 5 : [ x+5 | x <- fives2, (div_3 x+5) == False]


-- using the iterate function instead of list comprehensions
threes3 = iterate (+3) 0
fives3 = iterate (+5) 0

--could you calculate the fibonacci sequence like this:
--fib = [0 : 1 : a+b | ((first fib),  (first . rest fib))]
--looks like I need to use fibs and (tail fibs), at least. Does this work because the
--plus function takes the first element of a list? Probably a shortcut for (head fidbs) and (head tail fibs)
--

main = do
         --brute-force
         --  check every number from 3 to 1000
         print sum . filter (\x -> (div_3 x) || (div_5 x)) [3..1000]
         --smarter: generate list by multiplication
         --  i.e., 1,2,3 -> 3,6,9
         print (sum_first_1k threes1) + (sum_first_1k fives1)
         --using addition
         print (sum_first_1k threes2) + (sum_first_1k fives2)
         --using the iterate function
         print (sum_first_1k threes3) + (sum_first_1k fives3)
         --just for fun:
