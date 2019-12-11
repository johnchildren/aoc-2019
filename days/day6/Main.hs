module Main
  ( main
  )
where

import           Data.Either                              ( either
                                                          , lefts
                                                          , rights
                                                          )
import           Data.Maybe                               ( catMaybes )
import           Data.Tree
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

type Orbits = [(Text, Text)]
type OrbitTree = Tree Text

-- Catamorphism which keeps track of the number of child nodes seen
totalOrbits :: OrbitTree -> Int
totalOrbits = fst . foldTree
  (\_ xs ->
    let totalChildren = length xs + sum (snd <$> xs)
    in  (totalChildren + sum (fst <$> xs), totalChildren)
  )

type Transfer = Either (Maybe Int, Maybe Int) Int

-- fold the tree and count with some precedence rules
findTransfer :: Text -> Text -> OrbitTree -> Maybe Int
findTransfer start end tree = either (const Nothing) Just
  $ foldTree findTransfer' tree
 where
  findTransfer' :: Text -> [Transfer] -> Transfer
  findTransfer' l ts | l == start =
    case (catMaybes $ fst <$> lefts ts, catMaybes $ snd <$> lefts ts) of
      (_ , []   ) -> Left (Just 0, Nothing)
      ([], n : _) -> Right (n + 1)
  findTransfer' l ts | l == end =
    case (catMaybes $ fst <$> lefts ts, catMaybes $ snd <$> lefts ts) of
      ([]   , _ ) -> Left (Nothing, Just 0)
      (n : _, []) -> Right (n + 1)
  findTransfer' _ ts =
    case
        (catMaybes $ fst <$> lefts ts, catMaybes $ snd <$> lefts ts, rights ts)
      of
        ([]    , []    , []   ) -> Left (Nothing, Nothing)
        (n : _ , []    , _    ) -> Left (Just (n + 1), Nothing)
        ([]    , n : _ , _    ) -> Left (Nothing, Just (n + 1))
        (n1 : _, n2 : _, _    ) -> Right (n1 + n2)
        (_     , _     , n : _) -> Right n

buildOrbitTree :: Orbits -> OrbitTree
buildOrbitTree orbits = unfoldTree
  (\label -> (label, snd <$> filter (\(l, _) -> l == label) orbits))
  "COM"

parse :: Text -> Orbits
parse s =
  (\xs -> (head xs, head $ tail xs)) . Text.splitOn ")" <$> Text.lines s

main :: IO ()
main = do
  input <- Text.readFile "days/day6/input.txt"

  let orbits    = parse input
  let orbitTree = buildOrbitTree orbits

  --putStrLn $ drawTree (Text.unpack <$> orbitTree)

  putStr "part1: "
  print $ totalOrbits orbitTree

  putStr "part2: "
  print $ findTransfer "YOU" "SAN" orbitTree
