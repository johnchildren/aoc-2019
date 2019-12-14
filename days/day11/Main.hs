module Main
  ( main
  )
where

import qualified Data.List                     as List
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                               ( isNothing )
import qualified Data.Vector                   as Vector
import qualified Data.Text.IO                  as Text
import           IntCode                                  ( parse
                                                          , reserveMemory
                                                          , runStack
                                                          , Program
                                                          , ProgState
                                                          )

data Direction = DUp | DDown | DRight | DLeft
  deriving (Show)

data SystemState = SystemState { _grid :: Map (Int, Int) Int
                               , _robotLocation :: (Int, Int)
                               , _robotFacing :: Direction
                               , _programState :: Maybe ProgState
                               }
                               deriving (Show)

nextFacing :: Int -> Direction -> Direction
nextFacing 0 DUp    = DLeft
nextFacing 0 DLeft  = DDown
nextFacing 0 DDown  = DRight
nextFacing 0 DRight = DUp
nextFacing 1 DUp    = DRight
nextFacing 1 DRight = DDown
nextFacing 1 DDown  = DLeft
nextFacing 1 DLeft  = DUp

nextLocation :: (Int, Int) -> Direction -> (Int, Int)
nextLocation (x, y) DUp    = (x, y + 1)
nextLocation (x, y) DDown  = (x, y - 1)
nextLocation (x, y) DRight = (x + 1, y)
nextLocation (x, y) DLeft  = (x - 1, y)

runSystem :: SystemState -> SystemState
runSystem s@SystemState { _programState = Nothing } = s
runSystem SystemState { _grid = grid, _robotLocation = robotLocation, _robotFacing = robotFacing, _programState = Just programState }
  = let panel                  = Map.findWithDefault 0 robotLocation grid
        (output, newProgState) = runStack (Vector.singleton panel) programState
        o1                     = Vector.head output
        o2                     = Vector.head $ Vector.tail output
        newFacing              = nextFacing o2 robotFacing
    in  runSystem $ SystemState
          { _grid          = Map.insert robotLocation o1 grid
          , _robotLocation = nextLocation robotLocation newFacing
          , _robotFacing   = newFacing
          , _programState  = newProgState
          }

initialSystem :: Program -> SystemState
initialSystem prog = SystemState
  { _grid          = Map.empty
  , _robotLocation = (0, 0)
  , _robotFacing   = DUp
  , _programState  = Just (0, 0, reserveMemory prog 4096)
  }

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ []   = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = List.splitAt n list

render :: [Int] -> String
render image =
  unlines
    $   splitEvery 6
    $   (\case
          0 -> ' '
          1 -> '█'
        )
    <$> image

main :: IO ()
main = do
  input <- Text.readFile "days/day11/input.txt"
  let program    = parse input

  let finalState = runSystem $ initialSystem program

  Text.putStr "part1: "
  print $ Map.foldl (\x _ -> x + 1) 0 $ _grid finalState

  let finalState2 =
        runSystem $ (initialSystem program) { _grid = Map.singleton (0, 0) 1 }

  let finalPanels = fmap snd $ List.sortOn fst $ Map.toList $ Map.union
        (_grid finalState2)
        (Map.fromList [ ((x, y), 0) | x <- [0 .. 42], y <- [-5 .. 0] ])

  Text.putStrLn "part2: "
  putStrLn (render finalPanels)
