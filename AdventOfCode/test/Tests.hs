{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe, hspec)

import Program (run)

main :: IO ()
main = hspec $ do
  describe "add" $ do
      it "6+6=12" $ do
          run [1, 5, 6, 7, 99, 6, 6, 0] `shouldBe` [1, 5, 6, 7, 99, 6, 6, 12]
        
      it "102+1=12" $ do
          run [1, 5, 6, 7, 99, 102, 1, 0] `shouldBe` [1, 5, 6, 7, 99, 102, 1, 103]

      it "30+40=70" $ do
        run [1,9,10,3,99,3,11,0,99,30,40,50] `shouldBe` [1,9,10,70,99,3,11,0,99,30,40,50]

  describe "mult" $ do
    it "50*70=3500" $ do
      run [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]

  describe "small programs" $ do
    it "2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801)" $ do
      run [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      
    it "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99" $ do
      run [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]

    it "2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6)" $ do
      run [2,3,0,3,99] `shouldBe` [2,3,0,6,99]

    it "1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2)." $ do
      run [1,0,0,0,99] `shouldBe` [2,0,0,0,99]

    it "exploratory" $ do
      run [1,1,1,4,99] `shouldBe` [1,1,1,4,2]

  -- describe "the whole thing"
  --   it "works" $ do
  --     run [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,9,23,1,23,5,27,2,6,27,31,1,31,5,35,1,35,5,39,2,39,6,43,2,43,10,47,1,47,6,51,1,51,6,55,2,55,6,59,1,10,59,63,1,5,63,67,2,10,67,71,1,6,71,75,1,5,75,79,1,10,79,83,2,83,10,87,1,87,9,91,1,91,10,95,2,6,95,99,1,5,99,103,1,103,13,107,1,107,10,111,2,9,111,115,1,115,6,119,2,13,119,123,1,123,6,127,1,5,127,131,2,6,131,135,2,6,135,139,1,139,5,143,1,143,10,147,1,147,2,151,1,151,13,0,99,2,0,14,0]