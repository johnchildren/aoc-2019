module Main
  ( main
  )
where

import qualified Data.Text.IO                  as Text
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , runStack
                                                          , reserveMemory
                                                          )

main :: IO ()
main = do
  input <- Text.readFile "days/day9/input.txt"
  let program = parse input

  putStr "part1: "
  print $ fst $ runStack (Vector.singleton 1) (0, 0, reserveMemory program 4096)

  putStr "part2: "
  print $ fst $ runStack (Vector.singleton 5) (0, 0, reserveMemory program 4096)
