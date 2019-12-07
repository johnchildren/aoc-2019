module Main
  ( main
  )
where

import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.List                     as List
import           Data.IntMap                              ( IntMap )
import qualified Data.IntMap                   as IntMap

data Direction = U | D | R | L
  deriving (Show)

type Coordinate = (Int, Int)
type Distance = Int
type Steps = Int

data Move = Move Direction Distance
  deriving (Show)

-- X first grid
type Grid = IntMap (IntMap Steps)

emptyGrid :: Grid
emptyGrid = IntMap.empty

gridUnion :: Grid -> Grid -> Grid
gridUnion g1 g2 = IntMap.unionWith (IntMap.union) g1 g2

gridIntersection :: Grid -> Grid -> Grid
gridIntersection g1 g2 =
  IntMap.filter (not . IntMap.null)
    $ IntMap.intersectionWith (IntMap.intersectionWith (+)) g1 g2

gridCoordinates :: Grid -> [Coordinate]
gridCoordinates g =
  concatMap (\(x, ys) -> [ (x, y) | (y, _) <- ys ])
    $   IntMap.toList
    $   IntMap.toList
    <$> g

gridSteps :: Grid -> [(Coordinate, Steps)]
gridSteps g =
  concatMap (\(x, ys) -> [ ((x, y), s) | (y, s) <- ys ])
    $   IntMap.toList
    $   IntMap.toList
    <$> g

-- plot a single wire on the grid
plotWire :: (Coordinate, Steps) -> Move -> ((Coordinate, Steps), Grid)
plotWire ((x, y), s) (Move U distance) =
  ( ((x, y + distance), s + distance)
  , IntMap.singleton
    x
    (IntMap.fromList [ (y + d, s + d) | d <- [1 .. distance] ])
  )
plotWire ((x, y), s) (Move D distance) =
  ( ((x, y - distance), s + distance)
  , IntMap.singleton
    x
    (IntMap.fromList [ (y - d, s + d) | d <- [1 .. distance] ])
  )
plotWire ((x, y), s) (Move R distance) =
  ( ((x + distance, y), s + distance)
  , IntMap.fromList
    [ (x + d, IntMap.singleton y (s + d)) | d <- [1 .. distance] ]
  )
plotWire ((x, y), s) (Move L distance) =
  ( ((x - distance, y), s + distance)
  , IntMap.fromList
    [ (x - d, IntMap.singleton y (s + d)) | d <- [1 .. distance] ]
  )

plotRoute :: (Coordinate, Steps) -> [Move] -> Grid
plotRoute start moves =
  foldl gridUnion emptyGrid $ snd $ List.mapAccumL plotWire start moves

manhattanDistance :: Coordinate -> Int
manhattanDistance (x, y) = (abs x) + (abs y)

parse :: Text -> ([Move], [Move])
parse s =
  let allWires = (fmap parseMove . Text.splitOn ",") <$> Text.lines s
  in  (head allWires, head $ tail allWires) -- take only the first two
 where
  parseMove :: Text -> Move
  parseMove m =
    Move (parseDirection $ Text.head m) (read@Int . Text.unpack $ Text.tail m)

  parseDirection :: Char -> Direction
  parseDirection 'U' = U
  parseDirection 'D' = D
  parseDirection 'L' = L
  parseDirection 'R' = R
  parseDirection c   = error ("unknown direction: " ++ show c)

main :: IO ()
main = do
  input <- Text.readFile "days/day3/input.txt"
  let (wire1, wire2) = parse input
  let grid1          = plotRoute ((0, 0), 0) wire1
  let grid2          = plotRoute ((0, 0), 0) wire2

  let intersectionDistances =
        manhattanDistance <$> (gridCoordinates $ gridIntersection grid1 grid2)

  let shortestDistance = head $ List.sort intersectionDistances
  putStr "part1: "
  print shortestDistance

  let intersectionSteps = gridSteps $ gridIntersection grid1 grid2
  let shortestSteps =
        snd $ head $ List.sortOn (\((_, _), s) -> s) intersectionSteps
  putStr "part2: "
  print shortestSteps

