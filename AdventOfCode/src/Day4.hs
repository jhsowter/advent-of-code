module Day4 where
import Data.List

-- digits n = least : digits rest
--     where 
--         least = n `rem` 10
--         rest = n `div` 10

-- all (uncurry (<=)) $ pairs [1,2,3,4]

input :: [Int]
input = [382345..843167]

part1 :: Int
part1 = length $ findpass input

findpass :: [Int] -> [Int]
findpass s = filter match s

match :: Int -> Bool
match n = neverDec digits && hasAdj digits && isSixDigits n
    where 
        digits = getDigits n

isSixDigits :: Int -> Bool
isSixDigits n = n >= 100000 && n < 1000000


getDigits :: Int -> [Int]
getDigits num = [num `div` 10^p `rem` 10 | p <- [5,4..0]]

hasAdj :: [Int] -> Bool
hasAdj = not . null . filter ((==2) . length) . group

neverDec :: [Int] -> Bool
neverDec (x:y:rest) = x <= y && neverDec (y:rest)
neverDec [x] = True
neverDec [] = True

pairs :: [a] -> [(a, a)]
pairs l = zip l (drop 1 l)
