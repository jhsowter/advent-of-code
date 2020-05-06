module Program where
import Data.List (drop, take)
import Debug.Trace (trace)

data Opcode = ADD Mode Mode | MULT Mode Mode | INPUT | OUTPUT Mode | HALT | UNKNOWN
    deriving (Eq, Show)
        
data Mode = IMM | POS
    deriving (Eq, Show)

toMode :: Int -> Mode
toMode 0 = POS
toMode 1 = IMM

{-
ABCDE
 1002

DE - two-digit opcode,      02 == opcode 2
 C - mode of 1st parameter,  0 == position mode
 B - mode of 2nd parameter,  1 == immediate mode
 A - mode of 3rd parameter,  0 == position mode,
                                  omitted due to being a leading zero
-}

toOpcode  :: Int -> Opcode
toOpcode n = fromParts de c b a
    where
        de = n `mod` 100
        a = n `div` 10000 `rem` 10
        b = n `div` 1000 `rem` 10
        c = n `div` 100 `rem` 10

fromParts :: Int -> Int -> Int -> Int -> Opcode
fromParts 01 m1 m2 _ = ADD (toMode m1) (toMode m2)
fromParts 02 m1 m2 _ = MULT (toMode m1) (toMode m2)
fromParts 03 m1 m2 _ = INPUT 
fromParts 04 m1 m2 _ = OUTPUT (toMode m1)
fromParts 99 m1 m2 _ = HALT
fromParts _ _ _ _ = UNKNOWN

-- counter, memory, output
data State = State
    { counter :: Int
    , memory  :: [Int]
    , outputs :: [Int]
    }
    deriving (Show)

day2Input :: [Int]
day2Input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,9,23,1,23,5,27,2,6,27,31,1,31,5,35,1,35,5,39,2,39,6,43,2,43,10,47,1,47,6,51,1,51,6,55,2,55,6,59,1,10,59,63,1,5,63,67,2,10,67,71,1,6,71,75,1,5,75,79,1,10,79,83,2,83,10,87,1,87,9,91,1,91,10,95,2,6,95,99,1,5,99,103,1,103,13,107,1,107,10,111,2,9,111,115,1,115,6,119,2,13,119,123,1,123,6,127,1,5,127,131,2,6,131,135,2,6,135,139,1,139,5,143,1,143,10,147,1,147,2,151,1,151,13,0,99,2,0,14,0]

day5Input :: [Int]
day5Input = [3,225,1,225,6,6,1100,1,238,225,104,0,1002,92,42,224,1001,224,-3444,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,24,81,225,1101,89,36,224,101,-125,224,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,2,118,191,224,101,-880,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,68,94,225,1101,85,91,225,1102,91,82,225,1102,85,77,224,101,-6545,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1101,84,20,225,102,41,36,224,101,-3321,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1,188,88,224,101,-183,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1001,84,43,224,1001,224,-137,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1102,71,92,225,1101,44,50,225,1102,29,47,225,101,7,195,224,101,-36,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,344,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,359,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,374,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,419,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,434,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,464,1001,223,1,223,1007,677,226,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,226,226,224,102,2,223,223,1006,224,494,1001,223,1,223,8,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,524,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,539,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,554,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,569,101,1,223,223,108,226,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,599,1001,223,1,223,108,226,677,224,1002,223,2,223,1006,224,614,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,644,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,659,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226]

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
        (State _ final _) = run input
        input = store 2 verb $ store 1 noun memory

run :: [Int] -> State
run memory =  step firstState
    where
        firstState =  State 0 memory []
        -- firstOp = toOpcode (memory!!0)

step :: State -> State
-- step (State counter (ADD m1 m2) memory) = step $ runInstruction counter (binOp (+)) memory
-- step (State counter (MULT m1 m2) memory) = step $ runInstruction counter (binOp (*)) memory
-- step state@(State counter (HALT) memory) = state
-- step _ = error "unknown state"

step state@(State counter memory o) =
        case toOpcode instruction of
            ADD m1 m2 -> step  (binOp (+) m1 m2 state)
            MULT m1 m2 -> step (binOp (*) m1 m2 state)
            INPUT -> step $ input state
            OUTPUT mode -> step $ output mode state
            HALT -> state
            unknownState -> error $ "unknown state: " ++ show state
    where
        instruction = memory !! counter

-- runInstruction :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> State
-- runInstruction counter instruction memory = State nextCounter nextOp nextMemory
--     where 
--         nextCounter = counter + 4
--         nextMemory = instruction counter memory
--         nextOp = toOpcode $ nextMemory!!nextCounter
--         nextState = State nextCounter nextOp nextMemory

output :: Mode -> State -> State
output mode (State counter memory output) = State (counter+2) memory (output ++ [(fetch arg1 mode memory)])
    where 
        arg1 = memory!!(counter + 1)

input :: State -> State
input (State counter memory outputs) = State (counter+2) (store arg1 input memory) outputs
    where 
        input = 1
        arg1 = memory!!(counter + 1)


binOp :: (Int -> Int -> Int) -> Mode -> Mode -> State -> State
binOp operation mode1 mode2 (State counter memory outputs) = State (counter+4) (store address result memory) outputs
    where
        result = op1 `operation` op2
        op1 = fetch arg1 mode1 memory
        op2 = fetch arg2 mode2 memory

        arg1 = memory!!(counter + 1)
        arg2 = memory!!(counter + 2)
        address = memory!!(counter + 3)

fetch :: Int -> Mode -> [Int] -> Int
fetch a POS program = program!!a
fetch a IMM _ = a

store :: Int -> Int -> [Int] -> [Int]
store address value program
    | address < 0 = error "cannot store address < 0"
    | address + 1 > length program = error $ "cannot store address > program length: " ++ show address
store address value program = (take address program) ++ [value] ++ (drop (address+1) program)