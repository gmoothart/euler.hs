{- 
 - Sum the even fibonacci numbers that do not exceed 4 million
 - 
 - Is there a clever way to do this?
 -
 - -}
fib   :: Int -> Int
fib 0 =  0
fib 1 =  1
fib n =  (fib n-1) + fib(n-2)

flist = map (fib) [1..4000000]
evens = filter (\n -> (mod n 2) == 0) flist 
main = do
  print (sum (evens))
