module Main
  ( main
  )
where

import qualified Data.Vector                   as Vector
import           Test.Tasty
import           Test.Tasty.HUnit
import           IntCode                                  ( runStack )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [intCodeTests]

intCodeTests :: TestTree
intCodeTests = testGroup
  "IntCode tests"
  [ testCase "Day 2 Example"
  $   runStack
        Vector.empty
        (0, 0, (Vector.fromList [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))

  @?= (Vector.empty, Nothing)
  , testCase "Day 5 Example"
  $   runStack
        (Vector.singleton 8)
        ( 0
        , 0
        , Vector.fromList
          [ 3
          , 21
          , 1008
          , 21
          , 8
          , 20
          , 1005
          , 20
          , 22
          , 107
          , 8
          , 21
          , 20
          , 1006
          , 20
          , 31
          , 1106
          , 0
          , 36
          , 98
          , 0
          , 0
          , 1002
          , 21
          , 125
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 104
          , 999
          , 1105
          , 1
          , 46
          , 1101
          , 1000
          , 1
          , 20
          , 4
          , 20
          , 1105
          , 1
          , 46
          , 98
          , 99
          ]
        )
  @?= (Vector.singleton 1000, Nothing)
  ]
