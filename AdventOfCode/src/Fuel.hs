module Fuel (requiredFuel, totalRequired) where

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
zeroed = max 0
