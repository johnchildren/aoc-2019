{-# LANGUAGE TypeApplications #-}

module Main where

-- Fuel required to launch a given module is based on its mass.
-- Specifically, to find the fuel required for a module, take its mass,
-- divide by three, round down, and subtract 2.
fuel1 :: Int -> Int
fuel1 mass = (mass `div` 3) - 2

-- Fuel itself requires fuel just like a module - take its mass,
-- divide by three, round down, and subtract 2.
-- However, that fuel also requires fuel, and that fuel
-- requires fuel, and so on. Any mass that would require negative
-- fuel should instead be treated as if it requires zero fuel;
fuel2 :: Int -> Int
fuel2 mass =
  let stage = fuel1 mass in if stage > 0 then stage + fuel2 stage else 0

main :: IO ()
main = do
  input <- readFile "day1/input.txt"
  let masses     = read @Int <$> lines input
  let totalFuel1 = foldl (\f m -> f + fuel1 m) 0 masses
  putStr "part1: "
  print totalFuel1
  let totalFuel2 = foldl (\f m -> f + fuel2 m) 0 masses
  putStr "part2: "
  print totalFuel2
