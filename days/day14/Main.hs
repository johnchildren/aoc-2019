module Main
  ( main
  )
where

import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Tuple                    as Tuple
import           Data.Tree                                ( Tree )
import qualified Data.Tree                     as Tree

type Reactions = Map Text (Int, Map Text Int)
type ReactionTree = Tree (Text, Int, Int)

buildReactionTree :: Text -> Reactions -> ReactionTree
buildReactionTree rootChemical reactions = Tree.unfoldTree
  (\(chemical, required) ->
    ( ( chemical
      , required
      , fst $ Map.findWithDefault (0, Map.empty) chemical reactions
      )
    , Map.toList $ snd $ Map.findWithDefault (0, Map.empty) chemical reactions
    )
  )
  (rootChemical, 1)

totalChem :: ReactionTree -> Int
totalChem = Tree.foldTree (\(chemical, required, produced) xs -> undefined)

parse :: Text -> Reactions
parse s =
  Map.fromList
    $   uncurry parseEquation
    .   (\(lhs, rhs) -> (Text.drop 4 rhs, Text.splitOn ", " lhs))
    .   Text.breakOn " => "
    <$> Text.lines s
 where
  parseChemical :: Text -> (Int, Text)
  parseChemical =
    (\(quantity, chemical) ->
        (read @Int $ Text.unpack quantity, Text.drop 1 chemical)
      )
      . Text.breakOn " "

  parseEquation :: Text -> [Text] -> (Text, (Int, Map Text Int))
  parseEquation rhs lhs =
    let (prodQuantity, prodChemical) = parseChemical rhs
    in  ( prodChemical
        , (prodQuantity, Map.fromList $ Tuple.swap . parseChemical <$> lhs)
        )

main :: IO ()
main = do
  input <- Text.readFile "days/day14/input.txt"
  let reactions = parse input

  Text.putStr "part1: "
  putStrLn $ Tree.drawTree $ show <$> buildReactionTree "FUEL" reactions
