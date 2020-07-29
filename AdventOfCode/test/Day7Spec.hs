{-# LANGUAGE NamedFieldPuns #-}

module Day7Spec (spec) where 
import Day9
import Test.Hspec

spec :: Spec
spec = do
  describe "step" $ do
    it "== step' given day7 input" $ do
      let Halted state = runState' [0, 0] day7Input
      runState [0, 0] day7Input `shouldBe` state
      
    it "blocks when not given enough input" $ do
      let memory = [03, 1]
      runState' [] memory `shouldSatisfy` isBlocked
  describe "continue" $ do
    it "continues" $ do
      let Blocked blocked =  runState' []  [03, 1, 99]
      let Halted State { memory } = continue blocked [2]
      memory `shouldBe` [03, 2, 99]

  describe "loop fix" $ do
    it "" $ do
      let program = [3, 1, 4, 1, 3, 1, 4, 1, 99]
      let state = ampLoop program [5,6,7,8,9]
      state `shouldNotSatisfy` isBlocked 
      let Halted State { memory } = state
      memory `shouldBe`[3, 9, 4,  1, 3, 1, 4, 1, 99]

isBlocked :: StepResult -> Bool
isBlocked (Halted _) = False
isBlocked (Blocked _) = True

--       O-------O  O-------O  O-------O  O-------O  O-------O
-- 0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
--    |  O-------O  O-------O  O-------O  O-------O  O-------O |
--    |                                                        |
--    '--------------------------------------------------------+
--                                                             |
--                                                             v
--                                                      (to thrusters)