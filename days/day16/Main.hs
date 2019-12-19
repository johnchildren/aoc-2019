module Main
  ( main
  )
where

import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as Vector

phase :: Int -> Int -> Int
phase n p = phase' $ ((p + 1) `div` n) `rem` 4
 where
  phase' :: Int -> Int
  phase' 0 = 0
  phase' 1 = 1
  phase' 2 = 0
  phase' 3 = -1

modulate :: Vector Int -> Vector Int
modulate wf = Vector.imap
  (\i _ -> abs . (`rem` 10) . sum $ Vector.imap
    (\j x -> x * phase (i + 1) (i + j))
    (Vector.drop i wf)
  )
  wf

-- Find subsection of the wave assuming it is in the lower half
lrModulate :: Vector Int -> Vector Int
lrModulate = Vector.map (abs . (`rem` 10)) . Vector.scanr1' (+)

main :: IO ()
main = do
  input <- readFile "days/day16/input.txt"
  let wave       = read @Int . (: []) <$> head (lines input)
  let waveVector = Vector.fromList wave

  putStr "part1: "
  putStrLn $ concat $ show <$> Vector.take
    8
    (iterate modulate waveVector !! 100)

  let offset   = read @Int $ concat $ show <$> take 7 wave
  let fullWave = Vector.fromList $ take (10000 * length wave) $ cycle wave
  let partWave = Vector.drop offset fullWave

  let message = Vector.take 8 $ last $ take 101 $ iterate lrModulate partWave

  putStr "part2: "
  putStrLn $ concat $ show <$> message
