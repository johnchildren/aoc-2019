module Main
  ( main
  )
where


import           Data.Char                                ( chr )
import qualified Data.Text.IO                  as Text
import           Data.Vector                              ( Vector )
import qualified Data.Vector                   as Vector
import           IntCode                                  ( parse
                                                          , reserveMemory
                                                          , runStack
                                                          )

alignSum :: Vector (Vector Char) -> Int
alignSum grid = Vector.ifoldl'
  (\total i row ->
    total
      + Vector.ifoldl'
          (\subTotal j _ ->
            if isIntersect i j then (i * j) + subTotal else subTotal
          )
          0
          row
  )
  0
  grid
 where
  isIntersect :: Int -> Int -> Bool
  isIntersect i j =
    i
      /= 0
      && j
      /= 0
      && i
      +  2
      /= length grid
      && j
      +  2
      /= length (Vector.head grid)
      && isScaffold i       j
      && isScaffold (i - 1) j
      && isScaffold (i + 1) j
      && isScaffold i       (j - 1)
      && isScaffold i       (j + 1)

  isScaffold :: Int -> Int -> Bool
  isScaffold i j = (grid Vector.! i Vector.! j) == '#'

main :: IO ()
main = do
  input <- Text.readFile "days/day17/input.txt"
  let program = parse input

  let view = Vector.toList $ chr <$> fst
        (runStack Vector.empty (0, 0, reserveMemory program 8192))

  Text.putStr "part1: "
  print $ alignSum $ Vector.fromList $ Vector.fromList <$> lines view

  putStrLn view
