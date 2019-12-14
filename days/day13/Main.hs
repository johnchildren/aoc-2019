module Main
  ( main
  )
where

import           Data.Maybe                               ( catMaybes )
import qualified Data.Text.IO                  as Text
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , runStack
                                                          , reserveMemory
                                                          )

moveJoystick :: Int -> Int -> Int
moveJoystick ballX padelX = ballX - padelX

main :: IO ()
main = do
  input <- Text.readFile "days/day13/input.txt"
  let program     = parse input

  let (output, _) = runStack Vector.empty (0, 0, reserveMemory program 4096)
  let tiles = catMaybes $ Vector.toList $ Vector.imap
        (\i x -> if (i + 1) `mod` 3 == 0 then Just x else Nothing)
        output

  Text.putStr "part1: "
  print $ length $ filter (== 2) $ tiles

  print $ fst $ runStack (Vector.empty) (0, 0, reserveMemory (program Vector.// [(0, 2)]) 4096)
