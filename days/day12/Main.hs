module Main
  ( main
  )
where

import           Data.List                     as List
import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as Vector
import qualified Data.Set                      as Set
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

data Moon = Moon { _px, _py, _pz, _vx, _vy, _vz :: Int }
  deriving (Show)

type System = Vector Moon

data SystemAxis = SystemAxis { _ps, _vs :: Vector Int }
  deriving (Show, Eq, Ord)

velocityAdjustment :: Int -> Int -> Int
velocityAdjustment n m | n > m     = -1
                       | n < m     = 1
                       | otherwise = 0

applyGravity :: System -> System
applyGravity system = Vector.map
  (\m -> m
    { _vx =
      _vx m
        + ( Vector.sum
          $ Vector.map (\t -> velocityAdjustment (_px m) (_px t)) system
          )
    , _vy =
      _vy m
        + ( Vector.sum
          $ Vector.map (\t -> velocityAdjustment (_py m) (_py t)) system
          )
    , _vz =
      _vz m
        + ( Vector.sum
          $ Vector.map (\t -> velocityAdjustment (_pz m) (_pz t)) system
          )
    }
  )
  system

applyGravityVector :: Vector Int -> Vector Int -> Vector Int
applyGravityVector ps vs = Vector.imap
  (\i v -> v + (Vector.sum $ Vector.map (velocityAdjustment (ps Vector.! i)) ps)
  )
  vs

applyVelocity :: System -> System
applyVelocity = Vector.map
  (\m -> m { _px = _px m + _vx m, _py = _py m + _vy m, _pz = _pz m + _vz m })

applyVelocityVector :: Vector Int -> Vector Int -> Vector Int
applyVelocityVector ps vs = Vector.imap (\i p -> p + (vs Vector.! i)) ps

updateAxis :: SystemAxis -> SystemAxis
updateAxis axis =
  let nextVelocities = applyGravityVector (_ps axis) (_vs axis)
      nextPositions  = applyVelocityVector (_ps axis) nextVelocities
  in  axis { _ps = nextPositions, _vs = nextVelocities }

potentialEnergy :: Moon -> Int
potentialEnergy m = (abs $ _px m) + (abs $ _py m) + (abs $ _pz m)

kineticEnergy :: Moon -> Int
kineticEnergy m = (abs $ _vx m) + (abs $ _vy m) + (abs $ _vz m)

totalEnergy :: Moon -> Int
totalEnergy m = potentialEnergy m * kineticEnergy m

findPeriod :: Int -> SystemAxis -> Maybe Int
findPeriod maxIterations =
  fmap (\(_, period, _) -> period)
    . List.find (\(found, _, _) -> found)
    . List.take maxIterations
    . List.scanl
        (\(found, count, set) a -> if not found && not (Set.member a set)
          then (found, count + 1, Set.insert a set)
          else (True, count, set)
        )
        (False, 0, Set.empty)
    . List.iterate updateAxis

parse :: Text -> System
parse s =
  Vector.fromList
    $   (\coords -> Moon { _px = read . Text.unpack $ coords !! 0
                         , _py = read . Text.unpack $ coords !! 1
                         , _pz = read . Text.unpack $ coords !! 2
                         , _vx = 0
                         , _vy = 0
                         , _vz = 0
                         }
        )
    .   fmap (Text.drop 2)
    .   Text.splitOn ", "
    .   Text.dropAround (\c -> c == '<' || c == '>')
    <$> Text.lines s

main :: IO ()
main = do
  input <- Text.readFile "days/day12/input.txt"
  let system        = parse input

  let futureSystems = List.iterate (applyVelocity . applyGravity) system

  Text.putStr "part1: "
  print $ Vector.sum $ Vector.map totalEnergy (futureSystems !! 1001)

  let initialXAxis = SystemAxis { _ps = Vector.map _px system
                                , _vs = Vector.map _vx system
                                }
  let xPeriod = findPeriod 1000000 initialXAxis

  let initialYAxis = SystemAxis { _ps = Vector.map _py system
                                , _vs = Vector.map _vy system
                                }
  let yPeriod = findPeriod 1000000 initialYAxis

  let initialZAxis = SystemAxis { _ps = Vector.map _pz system
                                , _vs = Vector.map _vz system
                                }
  let zPeriod = findPeriod 1000000 initialZAxis

  Text.putStr "part2: "
  print $ lcm <$> (lcm <$> xPeriod <*> yPeriod) <*> zPeriod
