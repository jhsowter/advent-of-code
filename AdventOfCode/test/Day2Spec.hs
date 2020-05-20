module Day2Spec (spec) where 
import Program
import Test.Hspec

runP :: [Int] -> [Int]
runP m = toInts $ run m
toInts (State c m i o) = m


spec :: Spec
spec = do
  describe "add" $ do
    it "6+6=12" $ do
        runP [1, 5, 6, 7, 99, 6, 6, 0] `shouldBe` [1, 5, 6, 7, 99, 6, 6, 12]
      
    it "102+1=12" $ do
        runP [1, 5, 6, 7, 99, 102, 1, 0] `shouldBe` [1, 5, 6, 7, 99, 102, 1, 103]

    it "30+40=70" $ do
      runP [1,9,10,3,99,3,11,0,99,30,40,50] `shouldBe` [1,9,10,70,99,3,11,0,99,30,40,50]

  describe "mult" $ do
    it "50*70=3500" $ do
      runP [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
    it "3*6=18" $ do
      runP [2,6,1,5,99,0,3] `shouldBe` [2,6,1,5,99,18,3]
    it "19*9=171" $ do
      runP [2,5,6,23,99,19,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` [2,5,6,23,99,19,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,171]

  describe "small programs" $ do
    it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801)" $ do
      runP [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      
    it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $ do
      runP [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]

    it "2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6)" $ do
      runP [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

    it "1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2)." $ do
      runP [1,0,0,0,99] `shouldBe` [2,0,0,0,99]

  describe "the whole thing" $ do
    it "works" $ do
      runP [1,0,0,3,99] `shouldBe` [1,0,0,2,99]
      runP [1,1,2,3,99] `shouldBe` [1,1,2,3,99]
      runP [1,3,4,3,99] `shouldBe` [1,3,4,102,99]
      runP [1,5,0,3,99,3] `shouldBe` [1,5,0,4,99,3]
      runP [2,6,1,19,99,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0] `shouldBe` [2,6,1,19,99,0,3,0,0,0,0,0,0,0,0,0,0,0,0,18]