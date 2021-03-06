module Main
  ( main
  )
where

-- Validate that there are two adjacent identical digits
hasIdentical :: Int -> Bool
hasIdentical n = hasIdentical' (n, 10)
 where
  hasIdentical' :: (Int, Int) -> Bool
  hasIdentical' (0, _) = False
  hasIdentical' (d, l) =
    let (remaining, lastDigit) = quotRem d 10
    in  (lastDigit == l) || hasIdentical' (remaining, lastDigit)

-- Validate that each digit never decreases
neverDecreases :: Int -> Bool
neverDecreases n = neverDecreases' (n, 10)
 where
  neverDecreases' :: (Int, Int) -> Bool
  neverDecreases' (0, _) = True
  neverDecreases' (d, l) =
    let (remaining, lastDigit) = quotRem d 10
    in  (lastDigit <= l) && neverDecreases' (remaining, lastDigit)

-- Validate that there are exactly two adjacent identical digits
hasExactIdentical :: Int -> Bool
hasExactIdentical n = hasExactIdentical' (n, 10, 0)
 where
  hasExactIdentical' :: (Int, Int, Int) -> Bool
  hasExactIdentical' (0, _, t) = t == 1
  hasExactIdentical' (d, l, t) =
    let (remaining, lastDigit) = quotRem d 10
    in  if lastDigit == l
          then hasExactIdentical' (remaining, lastDigit, t + 1)
          else (t == 1) || hasExactIdentical' (remaining, lastDigit, 0)

main :: IO ()
main = do
  -- My puzzle input
  let (lower, upper) = (146810 :: Int, 612564 :: Int)
  let validPasswords1 =
        [ x | x <- [lower .. upper], neverDecreases x, hasIdentical x ]
  putStr "part1: "
  print (length validPasswords1)

  let validPasswords2 =
        [ x | x <- [lower .. upper], neverDecreases x, hasExactIdentical x ]
  putStr "part2: "
  print (length validPasswords2)
