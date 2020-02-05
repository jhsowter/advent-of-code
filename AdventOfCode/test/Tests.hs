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
        run [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [1,9,10,70,2,3,11,0,99,30,40,50]