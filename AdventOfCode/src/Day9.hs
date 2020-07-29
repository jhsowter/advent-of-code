{-# LANGUAGE NamedFieldPuns #-}


module Day9 where
import Data.List (drop, take)
import Debug.Trace (traceShowId, traceShow)

data Opcode = ADD Mode Mode Mode 
    | MULT Mode Mode Mode
    | INPUT Mode 
    | OUTPUT Mode 
    | JUMPTRUE Mode Mode 
    | JUMPFALSE Mode Mode 
    | LESSTHAN Mode Mode Mode
    | EQUAL Mode Mode Mode
    | RELBASEOFFSET Mode | HALT | UNKNOWN
    deriving (Eq, Show)
        
data Mode = IMM | POS | REL
    deriving (Eq, Show)
    
data StepResult = Halted State | Blocked State deriving Show

stepResultType :: StepResult -> String
stepResultType (Halted _state) = "halted"
stepResultType (Blocked _state) = "blocked"

toMode :: Int -> Mode
toMode 0 = POS
toMode 1 = IMM
toMode 2 = REL
toMode _ = error "wat"

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
fromParts 01 m1 m2 m3 = ADD (toMode m1) (toMode m2) (toMode m3)
fromParts 02 m1 m2 m3 = MULT (toMode m1) (toMode m2) (toMode m3)
fromParts 03 m1 _  _ = INPUT (toMode m1)
fromParts 04 m1 _  m3 = OUTPUT (toMode m1)
fromParts 05 m1 m2 m3 = JUMPTRUE (toMode m1) (toMode m2) 
fromParts 06 m1 m2 m3 = JUMPFALSE (toMode m1) (toMode m2) 
fromParts 07 m1 m2 m3 = LESSTHAN (toMode m1) (toMode m2) (toMode m3)
fromParts 08 m1 m2 m3 = EQUAL (toMode m1) (toMode m2) (toMode m3)
fromParts 09 m1 m2 m3 = RELBASEOFFSET (toMode m1)
fromParts 99 _  _  _ = HALT
fromParts _ _ _ _ = UNKNOWN

-- counter, memory, output
data State = State
    { counter :: Int
    , memory  :: [Int]
    , inputs :: [Int]
    , outputs :: [Int]
    , baseOffset :: Int
    }
    deriving (Show,Eq)

runState' :: [Int] -> [Int] -> StepResult
runState' inputs memory = step' State { counter = 0, memory, inputs, outputs = [], baseOffset=0 } 0

outputOf :: StepResult -> Int
outputOf (Blocked State {outputs}) = if length outputs /= 1 then error "too many outputs" else last outputs
outputOf (Halted State {outputs}) =  if length outputs /= 1 then error "too many outputs" else last outputs 

memoryOf :: StepResult -> [Int]
memoryOf (Blocked State {memory}) = memory
memoryOf (Halted State {memory}) = memory

step' :: State -> Int -> StepResult
step' state@(State counter memory i _ bo) cycles =
        case toOpcode instruction of
            ADD m1 m2 m3 -> step'  (binOp (+) m1 m2 m3 state) (cycles + 1)
            MULT m1 m2 m3 -> step' (binOp (*) m1 m2 m3 state) (cycles + 1)
            INPUT m1 -> if null i then Blocked state else step' (input state m1) (cycles + 1)
            OUTPUT mode -> step' (output mode state) (cycles + 1)
            JUMPTRUE m1 m2 -> step' (jump (/=) m1 m2 state) (cycles + 1)
            JUMPFALSE m1 m2 -> step' (jump (==) m1 m2 state) (cycles + 1)
            EQUAL m1 m2 m3 -> step' (binOp (\ x y -> if x == y then 1 else 0) m1 m2 m3 state) (cycles + 1)
            LESSTHAN m1 m2 m3 -> step' (binOp (\ x y -> if x < y then 1 else 0) m1 m2 m3 state) (cycles + 1)
            RELBASEOFFSET m1 -> step' (relBaseOffset state m1) (cycles + 1)
            HALT -> Halted state
            _ -> error $ "unknown state: " ++ show state
    where
        instruction = memory !! counter

relBaseOffset :: State -> Mode -> State
relBaseOffset state@(State counter memory i _ bo) m1 = state { baseOffset=bo + newOffset, counter = counter + 2 }
    where 
        newOffset = fetch (memory!!(counter+1)) m1 memory bo
-- 
jump :: (Int -> Int -> Bool) -> Mode -> Mode -> State -> State
jump compare' m1 m2 state@State { counter, memory, baseOffset } = if param1 `compare'` 0 then state { counter = param2 } else state { counter = counter + 3}
        where
            param1 = fetch arg1 m1 memory baseOffset
            param2 = fetch arg2 m2 memory baseOffset
            arg1 = memory!!(counter + 1)
            arg2 = memory!!(counter + 2)

output :: Mode -> State -> State
output mode state@State { counter, memory, outputs, baseOffset } = state { counter = counter+2, outputs = outputs ++ [(fetch arg1 mode memory baseOffset)]}
    where 
        arg1 = memory!!(counter + 1)

input :: State -> Mode -> State
input state@State{ counter, memory, inputs, baseOffset } mode = state { counter = counter+2, memory = store arg1 mode i baseOffset memory, inputs = remainingInputs }
    where 
        (i:remainingInputs) = inputs
        arg1 = memory!!(counter + 1)

binOp :: (Int -> Int -> Int) -> Mode -> Mode -> Mode -> State -> State
binOp operation mode1 mode2 mode3 state@State { counter, memory, baseOffset } = state { counter = counter + 4, memory = newMemory }
    where
        newMemory = store address mode3 result baseOffset memory
        result = op1 `operation` op2
        op1 = fetch arg1 mode1 memory baseOffset
        op2 = fetch arg2 mode2 memory baseOffset

        arg1 = memory!!(counter + 1)
        arg2 = memory!!(counter + 2)
        address = memory!!(counter + 3)

fetch :: Int -> Mode -> [Int] -> Int -> Int
fetch a POS program _ 
    | a < length program = program!!a
    | otherwise = 0
fetch a IMM _ _= a
fetch a REL program baseOffset
    | baseOffset + a < length program = program!!(baseOffset + a)
    | otherwise = 0

store :: Int -> Mode -> Int -> Int -> [Int] -> [Int]
store address POS value _ program = storePos address value program
store address REL value baseOffset program = storePos (baseOffset + address) value program
store _ IMM _ _  _ = error "cannot use IMM for store. idiot. "

storePos :: Int -> Int -> [Int] -> [Int]
storePos address value program
    | address < 0 = error "cannot store address < 0"
    | address + 1 > length program = (program ++ replicate (1 + address - length program) 0) // (address, value)
storePos address value program = program // (address, value)

(//) :: [a] -> (Int, a) -> [a]
list // (index, value) = listSet index value list

listSet :: Int -> a -> [a] -> [a]
listSet index value list = take index list ++ [value] ++ drop (index + 1) list