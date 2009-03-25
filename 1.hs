--
-- Euler project #1
-- The syntax is incorrect... but I know what I want to do!

--brute-force
--  check every number from 3 to 1000
nums = [3..1000]
print sum . filter . ((mod x 3) == 0 || (mod x 5) == 0) nums

--smarter
--  sum multiples of 3, and add multiples of 5 that are not also
--  multiples of 3
--  TODO: I think that this is not actually faster than the above. Instead I should do something like:
--        threes = [x*3 | x <= 1000 / 3]
--        Or, can I make it an infinite list, and specify the stopping condition at runtime?
threes = [x | (mod x 3) == 0 && x <= 1000]
fives = [x | (mod x 5) == 0 && (mod x 3) != 0 && x <= 1000]
print (sum threes) + (sum fives)


