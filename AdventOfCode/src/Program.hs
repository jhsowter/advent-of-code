module Program where
import Data.List (drop, take)

run :: [Int] -> [Int]
run ints@(h:t) = step 0 h ints

step :: Int -> Int -> [Int] -> [Int]
step counter 99 program = halt program
-- step counter 1 program = add counter program
step counter opcode program = step (counter+1) (program!!counter) program

halt :: [Int] -> [Int]
halt program = program

store :: Int -> Int -> [Int] -> [Int]
store 0 value program = [value] ++ drop 1 program
store address value program
    | address < 0 = error "cannot store address < 0"
    | address > length program = error "cannot store address > program length"
store address value program = (take i program) ++ [value] ++ (drop (i+1) program)
    where
        i = max 0 address