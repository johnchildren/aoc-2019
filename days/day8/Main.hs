module Main
  ( main
  )
where

import qualified Data.List                     as List
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ []   = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = List.splitAt n list

mergeChannel :: Int -> Int -> Int
mergeChannel 2 2 = 2
mergeChannel 2 1 = 1
mergeChannel 2 0 = 0
mergeChannel n _ = n

render :: [Int] -> String
render image =
  unlines
    $   splitEvery 25
    $   (\case
          0 -> ' '
          1 -> '█'
        )
    <$> image

parse :: (Int, Int) -> Text -> [[Int]]
parse (x, y) s = splitEvery (x * y) $ (\c -> read @Int $ [c]) <$> Text.unpack
  (List.head $ Text.lines s)

main :: IO ()
main = do
  input <- Text.readFile "days/day8/input.txt"
  let layers       = parse (25, 6) input

  let counts = fmap List.length . List.group . List.sort <$> layers
  let sortedCounts = List.sortOn head counts
  let leastZeros   = head sortedCounts

  putStr "part1: "
  print $ (leastZeros !! 1) * (leastZeros !! 2)

  let channels  = List.transpose layers
  let flatImage = List.foldl' mergeChannel 2 <$> channels

  putStrLn "part2: "
  putStrLn $ render flatImage
