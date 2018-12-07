--Given a file of the form
-- +a
-- -b
-- -c
--  ect
--  Add up all the numbers and return an output

data SignedInt = SignedInt [Char] Int

instance Show SignedInt where
 show (SignedInt sign num) = sign ++ (show num)

addSignedInts :: SignedInt -> SignedInt -> SignedInt
addSignedInts (SignedInt sign1 num1) (SignedInt sign2 num2)
 | sign1 == sign2               = SignedInt sign1 (num1 + num2)
 | sign1 == "+" && num1 > num2  = SignedInt sign1 (num1 - num2)
 | sign1 == "+" && num1 < num2  = SignedInt sign2 (num2 - num1)
 | sign2 == "+" && num1 > num2  = SignedInt sign1 (num1 - num2)
 | sign2 == "+" && num1 < num2  = SignedInt sign2 (num2 - num1)

stringToSigned :: String -> SignedInt
stringToSigned sign_num = SignedInt ([head sign_num]) ((read :: String -> Int) (tail sign_num))

main = do
 contents <- readFile "day1_input.txt"
 let string_list = words contents
 let signed_list = map stringToSigned string_list
 putStrLn $ show $ foldr addSignedInts (head signed_list) (tail signed_list)
 
