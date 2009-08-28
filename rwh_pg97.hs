import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

asInt :: String -> Integer 
asInt ""       = error "Not a Number"
asInt "-"      = error "Not a Number"
asInt ('-':xs) = - (asInt' xs)
asInt xs       = asInt' xs

step :: Integer -> Char -> Integer
step seed digit  
  | isDigit digit    = seed*10 + (toInteger (digitToInt digit))
  | otherwise        = error "Not a digit"

asInt' :: String -> Integer 
asInt' xs       = foldl' step 0 xs


--
-- Rewrite so caller can handle errors
--
type ErrorMessage = String
data Ei  = North Integer | South ErrorMessage

instance Show Ei where
  show (North x)  = "North " ++ (show x)
  show (South x)  = "South " ++ x

negate_either  :: Ei -> Ei
negate_either (North x)  = North (-x)
negate_either (South x)  = South x

asInt_either :: String -> Ei
asInt_either ""       = South "Not a Number"
asInt_either "-"      = South "Not a Number"
asInt_either ('-':xs) = negate_either (asInt_either' xs)
asInt_either xs       = asInt_either' xs

step_either :: Ei -> Char -> Ei
step_either (South a) digit    = South a  --propogate error  
step_either (North seed) digit =
  if isDigit digit
    then North (seed*10 + (toInteger (digitToInt digit)))
    else South "Not a digit"

asInt_either' :: String -> Ei 
asInt_either' xs       = foldl' step_either (North 0) xs

--
-- Concat
--

