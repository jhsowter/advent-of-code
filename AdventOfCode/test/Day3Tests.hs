{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Day3Tests where
import Test.Hspec        (Spec, describe, it, shouldBe, hspec)

import Day3

day3Tests :: IO ()
day3Tests = hspec $ do
    describe "manhattan" $ do
      it "for 2, 3 == 5" $ do
         taxi (1,1) (3,3) == 4