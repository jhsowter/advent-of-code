{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Test.Hspec        (Spec, describe, it, shouldBe, hspec)
import Day2Tests
import Day3Tests

main :: IO ()
main = hspec $ do
  day2Tests
  day3Tests
  