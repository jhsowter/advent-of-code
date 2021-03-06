{-# LANGUAGE NamedFieldPuns #-}

module Day5Spec (spec) where 
import Program
import Test.Hspec


runP :: [Int] -> [Int]
runP m = toInts $ run m
toInts (State c m i o) = m

spec :: Spec
spec = do
  describe "toOpcode tests" $ do
    it "0001" $ do
        toOpcode 0001 `shouldBe` ADD POS POS

    it "1101" $ do
        toOpcode 1101 `shouldBe` ADD IMM IMM

    it "1001" $ do
        toOpcode 1001 `shouldBe` ADD POS IMM

    it "0101" $ do
        toOpcode 0101 `shouldBe` ADD IMM POS

    it "0002" $ do
        toOpcode 0002 `shouldBe` MULT POS POS
        
    it "1102" $ do
        toOpcode 1102 `shouldBe` MULT IMM IMM

    it "0003" $ do
        toOpcode 0003 `shouldBe` INPUT

    it "1103" $ do
        toOpcode 1103 `shouldBe` INPUT

    it "0004" $ do
        toOpcode 0004 `shouldBe` OUTPUT POS

    it "1104" $ do
        toOpcode 1104 `shouldBe` OUTPUT IMM

  describe "programs" $ do
    it "1002,4,3,4,33" $ do
      runP [1002,4,3,4,33] `shouldBe` [1002, 4, 3, 4, 99]
    it "1001,4,3,5,99,33" $ do
      runP [1001,5,3,5,99,33] `shouldBe` [1001, 5, 3, 5, 99, 36]
      

  describe "examples" $ do  
    it "Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input was non-zero:" $ do
      outputs (runState [1] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) `shouldBe` [1]
      
    it "Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)." $ do
      outputs (runState [8] [3,9,8,9,10,9,4,9,99,-1,8]) `shouldBe` [1]
      outputs (runState [9] [3,9,8,9,10,9,4,9,99,-1,8]) `shouldBe` [0]

    it "Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)." $ do
      outputs (runState [8] [3,3,1108,-1,8,3,4,3,99]) `shouldBe` [1]
      outputs (runState [8] [3,9,7,9,10,9,4,9,99,-1,8]) `shouldBe` [0]
      outputs (runState [7] [3,9,7,9,10,9,4,9,99,-1,8]) `shouldBe` [1]