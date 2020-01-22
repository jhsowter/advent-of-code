module Main (requiredFuel, totalRequired, main) where

import System.IO (readLn)

main :: IO ()
main = do
  massLn <- getContents
  let masses = words massLn
  let total = totalRequired (map read masses)
  print total

fuelForMass :: Int -> Int
fuelForMass mass = div mass 3 - 2

totalRequired :: [Int] -> Int
totalRequired = sum . map requiredFuel

requiredFuel :: Int -> Int
requiredFuel 0 = 0
requiredFuel mass = fuel + extraFuel
  where
    fuel = zeroed $ fuelForMass mass
    extraFuel = zeroed $ requiredFuel fuel

zeroed :: Int -> Int
zeroed a = max 0 a
