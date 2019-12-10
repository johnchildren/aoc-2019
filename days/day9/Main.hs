module Main
  ( main
  )
where

import qualified Data.Text.IO                  as Text
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , run
                                                          )

main :: IO ()
main = do
  input <- Text.readFile "days/day9/input.txt"
  let program = parse input

  putStr "part1: "
  print $ fst $ run program (Vector.singleton 1)

  putStr "part2: "
  print $ fst $ run program (Vector.singleton 2)
