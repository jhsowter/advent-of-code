{-# LANGUAGE NamedFieldPuns #-}

module ProgramDay7 where
import Data.List (drop, take, maximum, permutations)
import Debug.Trace (traceShowId, trace)
import Control.Exception (assert)

data Opcode = ADD Mode Mode | MULT Mode Mode | INPUT | OUTPUT Mode | JUMPTRUE Mode Mode | JUMPFALSE Mode Mode | LESSTHAN Mode Mode | EQUAL Mode Mode | HALT | UNKNOWN
    deriving (Eq, Show)
        
data Mode = IMM | POS
    deriving (Eq, Show)
    
data StepResult = Halted State | Blocked State deriving Show

stepResultType (Halted _state) = "halted"
stepResultType (Blocked _state) = "blocked"

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
fromParts 05 m1 m2 _ = JUMPTRUE (toMode m1) (toMode m2) 
fromParts 06 m1 m2 _ = JUMPFALSE (toMode m1) (toMode m2) 
fromParts 07 m1 m2 _ = LESSTHAN (toMode m1) (toMode m2)
fromParts 08 m1 m2 _ = EQUAL (toMode m1) (toMode m2)
fromParts 99 m1 m2 _ = HALT
fromParts _ _ _ _ = UNKNOWN

-- counter, memory, output
data State = State
    { counter :: Int
    , memory  :: [Int]
    , inputs :: [Int]
    , outputs :: [Int]
    }
    deriving (Show,Eq)

day7Input :: [Int]
day7Input = [3,8,1001,8,10,8,105,1,0,0,21,42,55,64,77,94,175,256,337,418,99999,3,9,102,4,9,9,1001,9,5,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,5,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99]

eg1 :: [Int]
eg1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

eg2 :: [Int]
eg2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

eg2Phases :: [Int]
eg2Phases = [9,7,8,5,6]

eg1Phases :: [Int]
eg1Phases = [9,8,7,6,5]

runState :: [Int] -> [Int] -> State
runState inputs memory = step State { counter = 0, memory, inputs, outputs = [] }

continue :: State -> [Int] -> StepResult
continue state newInputs = step' state { inputs = newInputs }

runState' :: [Int] -> [Int] -> StepResult
runState' inputs memory = step' State { counter = 0, memory, inputs, outputs = [] }

largestAmp5 :: [Int] -> Int
largestAmp5 memory = maximum $ map (amp5 memory) ps
    where
        ps =  permutations [5..9]

--10737428485
largestAmp5' :: [Int] -> Int
largestAmp5' memory = maximum $ map (loopOut memory) ps
    where
        ps = permutations [5..9]
        
loopOut :: [Int] -> [Int] -> Int
loopOut memory phases = outputOf r
    where
        r = ampLoop memory phases

amp5 :: [Int] -> [Int] -> Int
amp5 memory phases = output5
    where
        [p1, p2, p3, p4, p5] = phases
        output1 = amp p1 0 memory
        output2 = amp p2 output1 memory
        output3 = amp p3 output2 memory
        output4 = amp p4 output3 memory 
        output5 = amp p5 output4 memory

ampLoop :: [Int] -> [Int] -> StepResult
ampLoop memory phases = final
    where
        [p1, p2, p3, p4, p5] = phases
        init1 = runState' [p1] memory
        init2 = runState' [p2] memory
        init3 = runState' [p3] memory
        init4 = runState' [p4] memory
        init5 = runState' [p5] memory
        [_, _, _, _, final] = looper' [init1, init2, init3, init4, init5] [0]


looper :: [StepResult] -> [Int] -> [StepResult]
looper amps@[_, _, _, _, a5] loopInput = case (a5) of
                Blocked state -> looper feedback [(outputOf l)]
                Halted state -> amps
    where
        feedback = (inner amps loopInput)
        [_, _, _, _, l] = feedback

looper' :: [StepResult] -> [Int] -> [StepResult]
looper' initialAmps initialInput = go (inner initialAmps initialInput)
    where
        go :: [StepResult] -> [StepResult]
        go amps@[a1, _, _, _, a5] =
            case a1 of
                Blocked state -> go (inner amps [outputOf a5])
                Halted state -> amps

inner :: [StepResult] -> [Int] -> [StepResult]
inner [init1, init2, init3, init4, init5] firstInput = [run1, run2, run3, run4, run5]
        where 
            run1 = case init1 of
                Blocked state -> continue state { outputs = [] } firstInput
                Halted state -> error "cannot feed input to halted amplifier 1"
            run2 = case init2 of
                Blocked state -> continue state { outputs = [] } [(outputOf run1)]
                Halted state -> error "cannot feed input to halted amplifier 2"
            run3 = case init3 of
                Blocked state -> continue state { outputs = [] } [(outputOf run2)]
                Halted state -> error "cannot feed input to halted amplifier 3"
            run4 = case init4 of
                Blocked state -> continue state { outputs = [] } [(outputOf run3)]
                Halted state -> error "cannot feed input to halted amplifier 4"
            run5 = case init5 of
                Blocked state -> continue state { outputs = [] } [(outputOf run4)]
                Halted state -> error "cannot feed input to halted amplifier 5"

-- continue :: StepResult -> [Int] -> StepResult
-- continue (Blocked state) newInputs = step' state { inputs=newInputs }
-- continue (Halted state) _ = error "cannot continue a halted machine"

outputOf :: StepResult -> Int
outputOf (Blocked State {outputs}) = if length outputs /= 1 then error "too many outputs" else last outputs
outputOf (Halted State {outputs}) =  if length outputs /= 1 then error "too many outputs" else last outputs 

memoryOf :: StepResult -> [Int]
memoryOf (Blocked State {memory}) = memory
memoryOf (Halted State {memory}) = memory

-- amp5' :: [Int] -> [Int] -> Int
-- amp5 memory phases = output5
--     where
--         [p1, p2, p3, p4, p5] = phases
--         result1 = runState [p1 0] memory
--         State { outputs = [output1] } = 
--         State { outputs = [output2] } = runState [p2 output1] memory
--         State { outputs = [output3] } = runState [p3 output2] memory
--         State { outputs = [output4] } = runState [p4 output3] memory
--         State { outputs = [output5] } = runState [p5 output4] memory

amp :: Int -> Int -> [Int] -> Int
amp phase input memory = output
    where 
        inputs = [phase, input]
        State { outputs = [output] } = runState inputs memory
        --[output] = outputs



execute :: Int -> Int -> [Int] -> Int
execute noun verb memory = output
    where 
        output = final!!0
        State { memory = final } = run input
        input = store 2 verb $ store 1 noun memory

run :: [Int] -> State
run memory =  step firstState
    where
        firstState =  State 0 memory [] []

step' :: State -> StepResult
step' state@(State counter memory i o) =
        case toOpcode instruction of
            ADD m1 m2 -> step'  (binOp (+) m1 m2 state)
            MULT m1 m2 -> step' (binOp (*) m1 m2 state)
            INPUT -> if null i then Blocked state else step' $ input state
            OUTPUT mode -> step' $ output mode state
            JUMPTRUE m1 m2 -> step' $ jump (/=) m1 m2 state
            JUMPFALSE m1 m2 -> step' $ jump (==) m1 m2 state
            EQUAL m1 m2 -> step' (binOp (\ x y -> if x == y then 1 else 0) m1 m2 state)
            LESSTHAN m1 m2 -> step' (binOp (\ x y -> if x < y then 1 else 0) m1 m2 state)
            HALT -> Halted state
            unknownState -> error $ "unknown state: " ++ show state
    where
        instruction = memory !! counter


step :: State -> State
step state@(State counter memory i o) =
        case toOpcode instruction of
            ADD m1 m2 -> step  (binOp (+) m1 m2 state)
            MULT m1 m2 -> step (binOp (*) m1 m2 state)
            INPUT -> step $ input state
            OUTPUT mode -> step $ output mode state
            JUMPTRUE m1 m2 -> step $ jump (/=) m1 m2 state
            JUMPFALSE m1 m2 -> step $ jump (==) m1 m2 state
            EQUAL m1 m2 -> step (binOp (\ x y -> if x == y then 1 else 0) m1 m2 state)
            LESSTHAN m1 m2 -> step (binOp (\ x y -> if x < y then 1 else 0) m1 m2 state)
            HALT -> state
            unknownState -> error $ "unknown state: " ++ show state
    where
        instruction = memory !! counter

-- 
jump :: (Int -> Int -> Bool) -> Mode -> Mode -> State -> State
jump compare m1 m2 state@State { counter, memory } = if param1 `compare` 0 then state { counter = param2 } else state { counter = counter + 3}
        where
            param1 = fetch arg1 m1 memory
            param2 = fetch arg2 m2 memory
            arg1 = memory!!(counter + 1)
            arg2 = memory!!(counter + 2)

output :: Mode -> State -> State
output mode state@State { counter, memory, outputs } = state { counter = counter+2, outputs = outputs ++ [(fetch arg1 mode memory)]}
    where 
        arg1 = memory!!(counter + 1)

input :: State -> State
input state@State{ counter, memory, inputs } = state { counter = counter+2, memory = store arg1 input memory, inputs = remainingInputs }
    where 
        (input:remainingInputs) = inputs
        arg1 = memory!!(counter + 1)

binOp :: (Int -> Int -> Int) -> Mode -> Mode -> State -> State
binOp operation mode1 mode2 state@State { counter, memory } = state { counter = counter + 4, memory = newMemory }
    where
        newMemory = store address result memory
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