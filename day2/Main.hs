{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.Text                                ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Vector                              ( (!)
                                                          , (//)
                                                          , Vector
                                                          )
import qualified Data.Vector                   as Vector

type Done = Bool

data Instr where
  Add :: Int -> Int -> Int -> Instr
  Mul :: Int -> Int -> Int -> Instr
  Terminate :: Instr

run :: Vector Int -> Vector Int
run = go 0
 where
  go pos vec = case step pos vec of
    (_      , nextVec, True ) -> nextVec
    (nextPos, nextVec, False) -> go nextPos nextVec

step :: Int -> Vector Int -> (Int, Vector Int, Done)
step pos vec = case readInstr pos vec of
  (0     , Terminate) -> (pos, vec, True)
  (offset, instr    ) -> (pos + offset, applyInstr instr vec, False)

readInstr :: Int -> Vector Int -> (Int, Instr)
readInstr pos vec = case vec ! pos of
  1  -> (4, Add (vec ! (pos + 1)) (vec ! (pos + 2)) (vec ! (pos + 3)))
  2  -> (4, Mul (vec ! (pos + 1)) (vec ! (pos + 2)) (vec ! (pos + 3)))
  99 -> (0, Terminate)

applyInstr :: Instr -> Vector Int -> Vector Int
applyInstr instr vec =
  let (op, lhs, rhs, res) = getOpAndPos instr
      val                 = (vec ! lhs) `op` (vec ! rhs)
  in  vec // [(res, val)]
 where
  getOpAndPos :: Instr -> (Int -> Int -> Int, Int, Int, Int)
  getOpAndPos (Add lhs rhs res) = ((+), lhs, rhs, res)
  getOpAndPos (Mul lhs rhs res) = ((*), lhs, rhs, res)

parse :: Text -> Vector Int
parse s = Vector.fromList $ read @Int . Text.unpack <$> Text.splitOn "," s

testParams :: Vector Int -> (Int, Int) -> Int
testParams program (noun, verb) = run (program // [(1, noun), (2, verb)]) ! 0

main :: IO ()
main = do
  input <- Text.readFile "day2/input.txt"
  let program         = parse input
  -- To do this, before running the program, replace position 1
  -- with the value 12 and replace position 2 with the value 2.
  let adjustedProgram = program // [(1, 12), (2, 2)]
  Text.putStr "part1: "
  let result = run adjustedProgram
  print (result ! 0)
  let values =
        [ (noun, verb)
        | noun <- [0 .. 99]
        , verb <- [0 .. 99]
        , testParams program (noun, verb) == 19690720
        ]
  let answer = (100 * fst (head values)) + snd (head values)
  Text.putStr "part2: "
  print answer
