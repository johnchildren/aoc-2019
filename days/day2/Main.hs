module Main
  ( main
  )
where

import qualified Data.Text.IO                  as Text
import           Data.Vector                              ( (!)
                                                          , (//)
                                                          , Vector
                                                          )
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , run
                                                          )

testParams :: Vector Int -> (Int, Int) -> Int
testParams program (noun, verb) =
  (snd $ run (program // [(1, noun), (2, verb)]) Vector.empty) ! 0

main :: IO ()
main = do
  input <- Text.readFile "days/day2/input.txt"
  let program         = parse input
  -- To do this, before running the program, replace position 1
  -- with the value 12 and replace position 2 with the value 2.
  let adjustedProgram = program // [(1, 12), (2, 2)]
  Text.putStr "part1: "
  let result = run adjustedProgram Vector.empty
  print ((snd result) ! 0)
  let values =
        [ (noun, verb)
        | noun <- [0 .. 99]
        , verb <- [0 .. 99]
        , testParams program (noun, verb) == 19690720
        ]
  let answer = (100 * fst (head values)) + snd (head values)
  Text.putStr "part2: "
  print answer
