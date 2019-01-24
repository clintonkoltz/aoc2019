-- Problem day 2 
-- Given a list of sting inputs. Count how many have a letter that repeats 2 and 3 times in each
-- Return the product of these 2 numbers

-- Function that passes though list and counts occurence for each letter

-- Map type will act as dictionary to hold the counts for each letter

{-# LANGUAGE FlexibleInstances #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe 

-- Only want to check if 2 or 3 count exist not the total amount.

letterCounts :: (Map Char Int) -> [Char] -> (Map Char Int)
letterCounts m ""                   = m
letterCounts m (s:ss)               = letterCounts newMap ss
 where newMap = Map.insert s newcount m
       newcount = fromMaybe 0 (Map.lookup s m) + 1 

find2n3 :: (Map Char Int) -> (Int, Int)
find2n3 m = (dupes, trips)
 where dupes = length $ Prelude.filter (==2) $ Map.elems m
       trips = length $ Prelude.filter (==3) $ Map.elems m

max1each :: (Int, Int) -> (Int, Int)
max1each (x,y) = (min 1 x, min 1 y)

addTuples :: (Int,Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- Part 1 main
--main = do
-- content <- readFile "day2_input.txt"
-- let lines = words content
-- putStrLn $ show 
--          $ uncurry (*)
--          $ Prelude.foldl addTuples (0,0) 
--	  $ Prelude.map max1each
--          $ Prelude.map find2n3 
--          $ Prelude.map (letterCounts Map.empty) lines
--

-- Part 2 is to find the two strings which are the closest alike
--  and return all the charaters they have in common
--
--

wordDistance :: String -> String -> Int
wordDistance s1 s2 = sum $ map (uncurry letterDistance) $ zip s1 s2

letterDistance :: Char -> Char -> Int
letterDistance c1 c2 
    | c1 == c2  = 0
    | otherwise =  1

data StringDistance = StringDistance (String, String, Int) deriving(Show)

instance Eq StringDistance where
  StringDistance (s1, s2, i1) == StringDistance (s3, s4, i2) = i1 == i2

instance Ord StringDistance where
  compare (StringDistance (s1, s2, i1)) (StringDistance (s3, s4, i2)) = compare i1 i2

closestWord :: [String] -> String -> StringDistance
closestWord [] s   = StringDistance (s, s, 0) 
closestWord [s1] s = StringDistance (s, s1, wordDistance s1 s) 
closestWord (s1:s2:ss) s 
  | (si1 <= si2) = closestWord (s1:ss) s
  | otherwise    = closestWord (s2:ss) s
    where si1    = wordDistance s s1
	  si2    = wordDistance s s2

allClosest :: [String] -> StringDistance
allClosest (s1:s2:[]) = closestWord [s1] s2
allClosest ss = min (closestWord (tail ss) (head ss)) (allClosest (tail ss))

returnSame :: Char -> Char -> Maybe Char
returnSame s1 s2 
  | (s1 == s2) = Just s1
  | otherwise  = Nothing

sameLetters :: String -> String -> String
sameLetters s1 s2 = catMaybes $ zipWith returnSame s1 s2

stringDistToString :: StringDistance -> (String, String)
stringDistToString (StringDistance (s1, s2, _)) = (s1, s2)

stringSame :: StringDistance -> String
stringSame sd = uncurry sameLetters (stringDistToString sd)


main :: IO ()
main = do
 content <- readFile "day2_input.txt"
 let lines = words content
 putStrLn $ show $ stringSame $ allClosest lines 

