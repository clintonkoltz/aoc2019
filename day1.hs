--Given a file of the form
-- +a
-- -b
-- -c
--  ect
--  Add up all the numbers and return an output

data SignedInt = SignedInt [Char] Int deriving(Eq)

instance Show SignedInt where
 show (SignedInt sign num) = sign ++ (show num)

addSignedInts :: SignedInt -> SignedInt -> SignedInt
addSignedInts (SignedInt sign1 num1) (SignedInt sign2 num2)
 | sign1 == sign2               = SignedInt sign1 (num1 + num2)
 | sign1 == "+" && num1 > num2  = SignedInt sign1 (num1 - num2)
 | sign1 == "+" && num1 < num2  = SignedInt sign2 (num2 - num1)
 | sign2 == "+" && num1 > num2  = SignedInt sign1 (num1 - num2)
 | sign2 == "+" && num1 < num2  = SignedInt sign2 (num2 - num1)
 | sign1 /= sign2 && num1 == 0  = SignedInt "+" 0
 | num1 == num2                 = SignedInt "+" 0

stringToSigned :: String -> SignedInt
stringToSigned sign_num = SignedInt ([head sign_num]) ((read :: String -> Int) (tail sign_num))

--main = do
-- contents <- readFile "day1_input.txt"
-- let string_list = words contents
-- let signed_list = map stringToSigned string_list
-- putStrLn $ show $ foldr addSignedInts (head signed_list) (tail signed_list)
 
-- Part 2 of the problem is to find the first partial sum to be repeated
-- May take multiple cycles of the list

-- Create a list of partial sums adding a number to the list if it is not already included.

partialSums :: [SignedInt] -> [SignedInt] -> SignedInt
paritalSums [] new = partialSums [SignedInt "+" 0] new
paritalSums stored [] = error ("List is empty before finishing")
partialSums stored new   
 | elem z stored = z
 | otherwise = partialSums (z:stored) (tail new)
 where z = addSignedInts (head stored) (head new)

main = do
 contents <- readFile "day1_input.txt"
 let string_list = words contents
 let signed_list = map stringToSigned string_list
 putStrLn $ show $ partialSums [SignedInt "+" 0] (cycle signed_list)
