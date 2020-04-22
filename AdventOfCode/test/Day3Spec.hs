{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Day3Spec (spec) where
import Test.Hspec

import Day3

spec :: Spec
spec = do
    describe "manhattan" $ do
      it "for 2, 3 == 5" $ do
         taxi (1,1) (3,3) == 4