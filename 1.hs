--
-- Euler project #1
-- The syntax is incorrect... but I know what I want to do!

--helpers
div_3 x = x mod 3 == 0
div_5 x = x mod 5 == 0
div_both x = (div_3 x) && (div_5 x)

--brute-force
--  check every number from 3 to 1000
nums = [3..1000]
print sum . filter . ((div_3 x) || (div_5 x)) nums

--smarter
--  sum multiples of 3, and add multiples of 5 that are not also
--  multiples of 3
--  TODO: I think that this is not actually faster than the above. Instead I should do something like:
--        threes = [x*3 | x <= 1000 / 3]
--        Or, can I make it an infinite list, and specify the stopping condition at runtime?
threes = [x | (mod x 3) == 0 && x <= 1000]
fives = [x | (mod x 5) == 0 && (mod x 3) != 0 && x <= 1000]
print (sum threes) + (sum fives)

-- here's another idea: build up the multiples recursively, by addition rather than multiplication.
-- ie, threes[5] = threes[4] + 4. Needs to be memoized to avoid duplication. Can I do this via a list
-- comprehension, or do I need recursive functions?


