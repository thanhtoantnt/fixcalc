{- | 
  Utility functions.
-}

module MyPrelude where
import Debug.Trace
import Numeric(fromRat,showFFloat)

tr :: Show a => String -> a -> a
tr msg x = trace (msg ++ show x ++ msg) x

tra:: Show a => String -> a -> a
tra msg x = trace msg x

singleton:: [a] -> Bool
singleton [x] = True
singleton _ = False

fst3:: (a,b,c) -> a
fst3 (a,b,c) = a

snd3:: (a,b,c) -> b
snd3 (a,b,c) = b

thd3:: (a,b,c) -> c
thd3 (a,b,c) = c

fst4 (a,b,c,d) = a
snd4 (a,b,c,d) = b
thd4 (a,b,c,d) = c
fth4 (a,b,c,d) = d

zipOrFail:: (Show a,Show b) => [a] -> [b] -> [(a,b)]
zipOrFail xs ys = 
  if (length xs == length ys) then zip xs ys
  else error $ "failed zip (args have different sizes): " ++ show xs ++ show ys

concatMapM:: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = 
  mapM f xs >>= \ys -> return (concat ys)

noElemFromFstIsInSnd:: Eq a => [a] -> [a] -> Bool
noElemFromFstIsInSnd [] ys = True
noElemFromFstIsInSnd (x:xs) ys = if x `elem` ys then False else noElemFromFstIsInSnd xs ys

numsFrom:: Int -> [Int]
numsFrom n = n:numsFrom (n+1)

updateList:: [a] -> Int -> a -> [a]
-- requires: 0<=i<length xs
updateList xs i val = take i xs ++ [val] ++ drop (i+1) xs

concatSepBy:: [a] -> [[a]] -> [a]
concatSepBy sep [] = []
concatSepBy sep [p] = p
concatSepBy sep (p:ps) = p ++ sep ++ (concatSepBy sep ps)

showDiffTimes:: Integer -> Integer -> String
showDiffTimes tEndCPU tStartCPU =
  let tAllCPU = fromRat $ (fromInteger (tEndCPU - tStartCPU)) / picoSec in
  (showFFloat Nothing tAllCPU " seconds")

picoSec :: Rational
picoSec = 1000000000000 -- 10^12

