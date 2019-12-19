module Main
  ( main
  )
where

import qualified Data.List                     as List
import           Data.Maybe                               ( catMaybes )
import qualified Data.Text.IO                  as Text
import qualified Data.Vector                   as Vector
import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set
import           IntCode                                  ( parse
                                                          , runStack
                                                          , reserveMemory
                                                          , ProgState
                                                          , Output
                                                          )

type Coordinate = (Int, Int)
type Visited = Set Coordinate

data MoveCommand = North | South | West | East
  deriving (Show)

move :: Coordinate -> MoveCommand -> Coordinate
move (x, y) North = (x, y + 1)
move (x, y) South = (x, y - 1)
move (x, y) West  = (x - 1, y)
move (x, y) East  = (x + 1, y)

encodeMoveCommand :: MoveCommand -> Int
encodeMoveCommand North = 1
encodeMoveCommand South = 2
encodeMoveCommand West  = 3
encodeMoveCommand East  = 4

-- Move around until the oxygen tank is found
findOxygen :: ProgState -> (Coordinate, [MoveCommand])
findOxygen state = go (Set.singleton (0, 0)) [((0, 0), [], state)]
 where
  go
    :: Visited
    -> [(Coordinate, [MoveCommand], ProgState)]
    -> (Coordinate, [MoveCommand])
  go visited states =
    let nextIteration = computeStep $ findValidSteps visited $ nextSteps states
    in  case isOxygen nextIteration of
          Just found -> found
          Nothing    -> go
            (nextVisited visited ((\(pos, _, _) -> pos) <$> nextIteration))
            (nonTerminated $ inEmptyTile nextIteration)

applyMoves :: ProgState -> [MoveCommand] -> ProgState
applyMoves state moves =
  case runStack (Vector.fromList $ encodeMoveCommand <$> moves) state of
    (_, Nothing       ) -> error "failed to apply moves"
    (_, Just progState) -> progState

gasExpansionTime :: ProgState -> Int
gasExpansionTime state = go 0 (Set.singleton (0, 0)) [((0, 0), [], state)]
 where
  go :: Int -> Visited -> [(Coordinate, [MoveCommand], ProgState)] -> Int
  go steps visited states =
    let nextIteration = computeStep $ findValidSteps visited $ nextSteps states
        nextValidStates = nonTerminated $ inEmptyTile nextIteration
    in  case length nextValidStates of
          0 -> steps
          _ -> go
            (steps + 1)
            (nextVisited visited ((\(pos, _, _) -> pos) <$> nextIteration))
            nextValidStates

nonTerminated
  :: [(Coordinate, [MoveCommand], (Output, Maybe ProgState))]
  -> [(Coordinate, [MoveCommand], ProgState)]
nonTerminated = catMaybes . fmap
  (\(coord, commands, (_, maybeProgState)) -> case maybeProgState of
    Just progState -> Just (coord, commands, progState)
    Nothing        -> Nothing
  )

nextVisited :: Visited -> [Coordinate] -> Visited
nextVisited visited coords = visited `Set.union` (Set.fromList coords)

inEmptyTile
  :: [(Coordinate, [MoveCommand], (Output, Maybe ProgState))]
  -> [(Coordinate, [MoveCommand], (Output, Maybe ProgState))]
inEmptyTile = filter (\(_, _, (out, _)) -> Vector.head out == 1)

isOxygen
  :: [(Coordinate, [MoveCommand], (Output, Maybe ProgState))]
  -> Maybe (Coordinate, [MoveCommand])
isOxygen = fmap (\(x, y, _) -> (x, y))
  . List.find (\(_, _, (out, _)) -> Vector.head out == 2)

computeStep
  :: [(Coordinate, [MoveCommand], ProgState)]
  -> [(Coordinate, [MoveCommand], (Output, Maybe ProgState))]
computeStep = map
  (\(pos, commands, progState) ->
    let newPosition = move pos (head commands)
    in  ( newPosition
        , commands
        , runStack (Vector.singleton $ encodeMoveCommand $ head commands)
                   progState
        )
  )

-- Don't move somewhere we have already visited
findValidSteps
  :: Visited
  -> [(Coordinate, [MoveCommand], ProgState)]
  -> [(Coordinate, [MoveCommand], ProgState)]
findValidSteps visited = filter
  (\(pos, commands, _) -> Set.notMember (move pos $ head commands) visited)

nextSteps
  :: [(Coordinate, [MoveCommand], ProgState)]
  -> [(Coordinate, [MoveCommand], ProgState)]
nextSteps = concatMap
  (\(pos, commands, progState) ->
    [ (pos, moveCommand : commands, progState)
    | moveCommand <- [North, South, East, West]
    ]
  )

main :: IO ()
main = do
  input <- Text.readFile "days/day15/input.txt"
  let program    = parse input

  let oxygenInfo = findOxygen (0, 0, reserveMemory program 8192)

  putStr "part1: "
  print $ length $ snd $ oxygenInfo

  putStr "part2: "
  print $ gasExpansionTime
    (applyMoves (0, 0, reserveMemory program 8192) (reverse $ snd oxygenInfo))
