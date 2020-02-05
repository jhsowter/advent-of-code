module Program where
import Data.List (drop, take)

run :: [Int] -> [Int]
run [] = []
run memory@(op:_) = step 0 op memory

step :: Int -> Int -> [Int] -> [Int]
step counter 99 memory = halt memory
step counter 1 memory = step nextCounter nextOp nextState
    where
        nextCounter = counter + 3
        nextState = add counter memory
        nextOp = memory!!nextCounter
step counter opcode memory = step (counter+1) (memory!!counter) memory

add :: Int -> [Int] -> [Int]
add counter program = store address result program
    where
        result = op1 + op2
        op1 = fetch arg1 program
        op2 = fetch arg2 program
        arg1 = program!!(counter + 1)
        arg2 = program!!(counter + 2)
        address = program!!(counter + 3)


halt :: [Int] -> [Int]
halt program = program

fetch :: Int -> [Int] -> Int
fetch address program
    | address > length program = error $ "cannot fetch address " ++ show address
    | address < 0 = error "address less than zero"
fetch address program = program!!address


store :: Int -> Int -> [Int] -> [Int]
store 0 value program = [value] ++ drop 1 program
store address value program
    | address < 0 = error "cannot store address < 0"
    | address > length program = error $ "cannot store address > program length: " ++ show address
store address value program = (take i program) ++ [value] ++ (drop (i+1) program)
    where
        i = max 0 address