module Main
  ( main
  )
where

import qualified Data.Text.IO                  as Text
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , runStack
                                                          , reserveMemory
                                                          , ProgState
                                                          )

data Status = Stationary | Pulled
  deriving (Show, Eq)

readStatus :: Int -> Status
readStatus 0 = Stationary
readStatus 1 = Pulled

checkPoint :: ProgState -> (Int, Int) -> Status
checkPoint progState (x, y) =
  readStatus $ Vector.head $ fst $ runStack (Vector.fromList [x, y]) progState

main :: IO ()
main = do
  input <- Text.readFile "days/day19/input.txt"
  let program   = parse input
  let progState = (0, 0, reserveMemory program 4096)

  let locations =
        [ ((x, y), checkPoint progState (x, y))
        | x <- [0 .. 49]
        , y <- [0 .. 49]
        ]

  Text.putStr "part1: "
  print $ length $ filter (\(_, status) -> status == Pulled) locations
