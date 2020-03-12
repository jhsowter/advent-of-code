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

-- from https://adventofcode.com/2019/day/2/input
runProgram :: State
runProgram = run 12 2 [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,9,23,1,23,5,27,2,6,27,31,1,31,5,35,1,35,5,39,2,39,6,43,2,43,10,47,1,47,6,51,1,51,6,55,2,55,6,59,1,10,59,63,1,5,63,67,2,10,67,71,1,6,71,75,1,5,75,79,1,10,79,83,2,83,10,87,1,87,9,91,1,91,10,95,2,6,95,99,1,5,99,103,1,103,13,107,1,107,10,111,2,9,111,115,1,115,6,119,2,13,119,123,1,123,6,127,1,5,127,131,2,6,131,135,2,6,135,139,1,139,5,143,1,143,10,147,1,147,2,151,1,151,13,0,99,2,0,14,0]
    where 
        
run :: Int -> Int -> [Int] -> State
run p1 p2 memory = final
    where
        final = step $ State 0 firstOp input
        firstOp = fromInt (input!!0)
        input = store 2 p2 $ store 1 p1 memory

complete :: State -> [Int]
complete (State c op s) = s

step :: State -> State
step (State counter _ memory)
    | counter > length memory = error "program counter exceeded the length of the program"
step (State counter ADD memory)
    | counter + 4 > length memory = error "program counter exceeded the length of the program"
step (State counter MULT memory)
    | counter + 4 > length memory = error "program counter exceeded the length of the program"

step (State counter ADD memory) = step nextState
    where
        nextCounter = counter + 4
        nextMemory = add counter memory
        nextOp = fromInt $ nextMemory!!nextCounter
        nextState = State nextCounter nextOp nextMemory
        
step (State counter MULT memory) = step nextState
    where
        nextCounter = counter + 4
        nextMemory = mult counter memory
        nextOp = fromInt $ nextMemory!!nextCounter
        nextState = State nextCounter nextOp nextMemory

step state = state
state (State counter UNKNOWN memory) = error "unknown state"

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
store 0 value program = [value] ++ drop 1 program
store address value program
    | address < 0 = error "cannot store address < 0"
    | address + 1 > length program = error $ "cannot store address > program length: " ++ show address
store address value program = (take i program) ++ [value] ++ (drop (i+1) program)
    where
        i = max 0 address