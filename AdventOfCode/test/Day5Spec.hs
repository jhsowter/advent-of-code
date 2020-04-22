module Day5Spec (spec) where 
import Program
import Test.Hspec


runP :: [Int] -> [Int]
runP m = toInts $ run m
toInts (State c op m) = m

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
        toOpcode 0003 `shouldBe` INPUT POS

    it "1103" $ do
        toOpcode 1103 `shouldBe` INPUT IMM

    it "0004" $ do
        toOpcode 0004 `shouldBe` OUTPUT POS

    it "1104" $ do
        toOpcode 1104 `shouldBe` OUTPUT IMM


  describe "programs" $ do
    it "1002,4,3,4,33" $ do
      runP [1002,4,3,4,33] `shouldBe` [1002, 4, 3, 4, 99]