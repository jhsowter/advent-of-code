module Program where
import Data.List (drop, take)
import Debug.Trace (trace)

data Opcode = ADD | MULT | HALT | UNKNOWN
    deriving Show

fromInt  :: Int -> Opcode
fromInt 1 = ADD
fromInt 2 = MULT
fromInt 99 = HALT
fromInt _ = UNKNOWN

-- counter, opcode, memory
data State = State Int Opcode [Int]
    deriving (Show)

day2Input :: [Int]
day2Input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,9,23,1,23,5,27,2,6,27,31,1,31,5,35,1,35,5,39,2,39,6,43,2,43,10,47,1,47,6,51,1,51,6,55,2,55,6,59,1,10,59,63,1,5,63,67,2,10,67,71,1,6,71,75,1,5,75,79,1,10,79,83,2,83,10,87,1,87,9,91,1,91,10,95,2,6,95,99,1,5,99,103,1,103,13,107,1,107,10,111,2,9,111,115,1,115,6,119,2,13,119,123,1,123,6,127,1,5,127,131,2,6,131,135,2,6,135,139,1,139,5,143,1,143,10,147,1,147,2,151,1,151,13,0,99,2,0,14,0]

test :: Int -> Int -> Bool
test x y = execute x y day2Input == 19690720

findIn :: Int -> [(Int,Int)]
findIn n = take n [(x,y) | x <- [0..99], y <- [0..99]]

findNumber :: (Int, Int)
findNumber = find (findIn 9000)

find :: [(Int, Int)] -> (Int, Int)
find [] = (-1, -1)
find (h:t) = if found then h else find t
    where 
        (x,y) = h
        found = test x y

-- from https://adventofcode.com/2019/day/2/input
runProgram :: Int
runProgram = execute 12 2 day2Input

execute :: Int -> Int -> [Int] -> Int
execute noun verb memory = output
    where 
        output = final!!0
        (State _ _ final) = run input
        input = store 2 verb $ store 1 noun memory

run :: [Int] -> State
run memory =  step firstState
    where
        firstState =  State 0 firstOp memory
        firstOp = fromInt (memory!!0)

step :: State -> State
step (State counter _ memory)
    | counter > length memory = error "program counter exceeded the length of the program"
step (State counter ADD memory)
    | counter + 4 > length memory = error "program counter exceeded the length of the program"
step (State counter MULT memory)
    | counter + 4 > length memory = error "program counter exceeded the length of the program"
step (State counter ADD memory) = step $ runInstruction counter add memory
step (State counter MULT memory) = step $ runInstruction counter mult memory
step state = state
state (State counter UNKNOWN memory) = error "unknown state"

runInstruction :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> State
runInstruction counter instruction memory = State nextCounter nextOp nextMemory
    where 
        nextCounter = counter + 4
        nextMemory = instruction counter memory
        nextOp = fromInt $ nextMemory!!nextCounter
        nextState = State nextCounter nextOp nextMemory

add :: Int -> [Int] -> [Int]
add counter program = store address result program
    where
        result = op1 + op2
        op1 = fetch arg1 program
        op2 = fetch arg2 program
        arg1 = program!!(counter + 1)
        arg2 = program!!(counter + 2)
        address = program!!(counter + 3)

mult :: Int -> [Int] -> [Int]
mult counter memory = store address result memory
    where   
        result = op1 * op2
        op1 = fetch arg1 memory
        op2 = fetch arg2 memory
        arg1 = memory!!(counter + 1)
        arg2 = memory!!(counter + 2)
        address = memory!!(counter + 3)

fetch :: Int -> [Int] -> Int
fetch address program
    | address > length program = error $ "cannot fetch address " ++ show address
    | address < 0 = error "address less than zero"
fetch address program = program!!address

store :: Int -> Int -> [Int] -> [Int]
store address value program
    | address < 0 = error "cannot store address < 0"
    | address + 1 > length program = error $ "cannot store address > program length: " ++ show address
store address value program = (take address program) ++ [value] ++ (drop (address+1) program)