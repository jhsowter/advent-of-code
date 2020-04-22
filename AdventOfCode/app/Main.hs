module Main (requiredFuel, totalRequired, main) where

import System.IO (readLn)
import Fuel
import Program

main :: IO ()
main = do
  massLn <- getContents
  let masses = words massLn
  let total = totalRequired (map read masses)
  print total