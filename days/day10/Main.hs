module Main
  ( main
  )
where

import           Data.Maybe                               ( catMaybes )
import qualified Data.Vector                   as Vector
import qualified Data.List                     as List

truncate3 :: Double -> Double
truncate3 = (/ 10000) . fromIntegral . round . (* 10000)

mag :: (Int, Int) -> Double
mag (x, y) = sqrt $ fromIntegral $ (x ^ 2) + (y ^ 2)

norm :: (Int, Int) -> (Double, Double)
norm (x, y) =
  let magnitude = mag (x, y)
  in  ( truncate3 $ fromIntegral x / magnitude
      , truncate3 $ fromIntegral y / magnitude
      )

blocks :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
blocks (ox, oy) (bx, by) (tx, ty) =
  let toBlocker = (bx - ox, by - oy)
      toTarget  = (tx - ox, ty - oy)
  in  norm toBlocker == norm toTarget && mag toBlocker < mag toTarget

visibleAsteroids :: [(Int, Int)] -> [Int]
visibleAsteroids asteroids = map
  (\origin -> flip (-) 1 $ sum $ map
    (\blocker -> if all (not . blocks origin blocker) asteroids then 1 else 0)
    asteroids
  )
  asteroids

-- Clockwise view of asteroids, closest first
circularView :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
circularView (ox, oy) asteroids =
  let relativeLocations = (\(x, y) -> (x - ox, y - oy)) <$> asteroids
  in  reverse
        $ fmap (fmap fst . List.sortOn (\(_, v) -> mag v))
        $ List.groupBy (\(_, v1) (_, v2) -> norm v1 == norm v2)
        $ List.sortOn (\(_, (x, y)) -> atan2 (fromIntegral x) (fromIntegral y))
        $ zip asteroids relativeLocations

parse :: String -> [(Int, Int)]
parse s =
  catMaybes
    $   concat
    $   Vector.toList
    $   fmap Vector.toList
    $   Vector.imap
          (\y xs -> Vector.imap
            (\x c -> case c of
              '#' -> Just (x, y)
              _   -> Nothing
            )
            xs
          )
    $   Vector.fromList
    $   Vector.fromList
    <$> lines s

main :: IO ()
main = do
  input <- readFile "days/day10/input.txt"
  let asteroids = parse input
  let (bestLocation, numberVisible) =
        last $ List.sortOn snd $ zip asteroids (visibleAsteroids asteroids)

  putStr "part1: "
  print numberVisible

  putStr "part2: "
  print
    $ (\(x, y) -> (x * 100) + y)
    $ flip (!!) 200
    $ List.concat
    $ List.transpose
    $ circularView bestLocation asteroids
