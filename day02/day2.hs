{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List

test = map parse [ "A Y"
                 , "B X"
                 , "C Z"
                 ]

input = map parse . lines <$> readFile "input.txt"

data Play = A | B | C
  deriving (Show, Read, Eq, Enum)

data Unknown = X | Y | Z
  deriving (Show, Read, Eq, Enum)

type Game = (Play, Unknown)

parse :: String -> Game
parse line = (read w1, read w2)
  where [w1, w2] = words line

shapeValue :: Unknown -> Int
shapeValue = \case X -> 1 ; Y -> 2 ; Z -> 3

part1Score (play, unk) = shapeValue unk + outcome
  where outcome = case (play, unk) of
                    (A, Z) -> 0
                    (C, Y) -> 0
                    (B, X) -> 0
                    _ | fromEnum play == fromEnum unk -> 3
                    _ -> 6

part1 input = sum $ map part1Score input

answer1 = part1 <$> input

shapeValue2 :: Play -> Int
shapeValue2 p = fromEnum p + 1

getPlay :: Game -> Play
getPlay = \case
  (p, Y) -> p
  (A, X) -> C
  (C, X) -> B
  (B, X) -> A
  (A, Z) -> B
  (C, Z) -> A
  (B, Z) -> C

part2Score game@(play, unk) = shapeValue2(getPlay game) + (case unk of X -> 0; Y -> 3; Z -> 6)

part2 input = sum $ map part2Score input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
