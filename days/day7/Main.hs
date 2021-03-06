module Main
  ( main
  )
where

import           Data.List                     as List
import           Data.Maybe                               ( catMaybes )
import qualified Data.Text.IO                  as Text
import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , runStack
                                                          , ProgState
                                                          )

thrust :: Vector Int -> [Int] -> Int
thrust program = foldl'
  (\x y -> Vector.head $ fst $ runStack (Vector.fromList [y, x]) (0, 0, program)
  )
  0

thrustPropagated :: Vector Int -> [Int] -> Int
thrustPropagated initialProg config = fst $ go 0 initialAmps
 where
  initialAmps :: [Maybe ProgState]
  initialAmps =
    snd . (`runStack` (0, 0, initialProg)) . Vector.singleton <$> config

  go :: Int -> [Maybe ProgState] -> (Int, [Maybe ProgState])
  go sig (catMaybes -> []) = (sig, [])
  go sig amps              = uncurry go (pass sig amps)

  pass :: Int -> [Maybe ProgState] -> (Int, [Maybe ProgState])
  pass = List.mapAccumL runAmp

  runAmp :: Int -> Maybe ProgState -> (Int, Maybe ProgState)
  runAmp sig Nothing   = (sig, Nothing)
  runAmp sig (Just st) = case runStack (Vector.singleton sig) st of
    (sigOut, Just stateOut) -> (Vector.head sigOut, Just stateOut)
    (sigOut, Nothing      ) -> (Vector.head sigOut, Nothing)

main :: IO ()
main = do
  input <- Text.readFile "days/day7/input.txt"
  let program = parse input

  putStr "part1: "
  let highestSignal1 = maximum $ thrust program <$> List.permutations [0 .. 4]
  print highestSignal1


  putStr "part2: "
  let highestSignal2 =
        maximum $ thrustPropagated program <$> List.permutations [5 .. 9]
  print highestSignal2
