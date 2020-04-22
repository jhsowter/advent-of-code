module Day5Spec (spec) where 
import Program
import Test.Hspec


runP :: [Int] -> [Int]
runP m = toInts $ run m
toInts (State c op m) = m

spec :: Spec
spec = do
  describe "toOpcode tests" $ do
    it "0002 means MULT, IMM,   " $ do
        toOpcode 0002 `shouldBe` MULT POS POS
