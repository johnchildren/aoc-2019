module Main
  ( main
  )
where

import           Data.Maybe                               ( fromMaybe
                                                          , catMaybes
                                                          )
import qualified Data.Text.IO                  as Text
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , runStack
                                                          , reserveMemory
                                                          , ProgState
                                                          , Output
                                                          )

moveJoystick :: Int -> Int -> Int
moveJoystick ballX paddelX = paddelX - ballX

findPaddleX :: Output -> Maybe Int
findPaddleX output =
  (\(i, _) -> (output Vector.! (i - 2)))
    <$> (Vector.find (\(_, x) -> x == 3) $ Vector.filter
          (\(i, _) -> (i + 1) `mod` 3 == 0)
          (Vector.indexed output)
        )

findBallX :: Output -> Maybe Int
findBallX output =
  (\(i, _) -> (output Vector.! (i - 2)))
    <$> (Vector.find (\(_, x) -> x == 4) $ Vector.filter
          (\(i, _) -> (i + 1) `mod` 3 == 0)
          (Vector.indexed output)
        )

playGame :: ProgState -> Output
playGame = go 0
 where
  go :: Int -> ProgState -> Output
  go js st = case runStack (Vector.singleton js) st of
    (out, Nothing   ) -> out
    (out, Just newSt) -> go
      (fromMaybe js $ moveJoystick <$> findPaddleX out <*> findBallX out)
      newSt

main :: IO ()
main = do
  input <- Text.readFile "days/day13/input.txt"
  let program     = parse input

  let (output, _) = runStack Vector.empty (0, 0, reserveMemory program 4096)
  let tiles = catMaybes $ Vector.toList $ Vector.imap
        (\i x -> if (i + 1) `mod` 3 == 0 then Just x else Nothing)
        output

  Text.putStr "part1: "
  print $ length $ filter (== 2) tiles

  Text.putStr "part2: "
  -- abusing the fact that I know the last number in the input is the high score
  print $ Vector.last $ playGame
    (0, 0, reserveMemory (program Vector.// [(0, 2)]) 4096)
