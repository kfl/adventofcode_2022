{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import qualified Data.Set as S
import Data.Set (Set)



test =  map parse [ "R 4"
                  , "U 4"
                  , "L 3"
                  , "D 1"
                  , "R 4"
                  , "D 1"
                  , "L 5"
                  , "R 2"
                  ]

input = map parse . lines <$> readFile "input.txt"

data Direction = R | L | U | D
  deriving (Read, Show, Eq, Enum)

type Command = (Direction, Int)

parse :: String -> Command
parse line = (read d, read n)
  where [d, n] = words line


type History = Set Pos
type Pos = (Int, Int)
type State = (Pos, Pos, History)

moveDir (i, j) = \case
  R -> (i, j+1)
  L -> (i, j-1)
  U -> (i+1, j)
  D -> (i-1, j)

catchUp :: Pos -> Pos -> Pos
catchUp hp@(hi, hj) = \case
  tp@(ti, tj) | abs(hi-ti) <= 1 && abs(hj-tj) <= 1 -> tp
  (ti, tj) -> (ti + signum (hi - ti), tj + signum (hj - tj))

step :: Direction -> State -> State
step dir (h, t, hist) = (h', t', S.insert t' hist)
  where h' = moveDir h dir
        t' = catchUp h' t

type Stepper a = Direction -> a -> a

move :: Stepper a -> a -> Command -> a
move step s (dir, n) = iterate (step dir) s !! n

part1 :: [Command] -> Int
part1 input = S.size final
  where initial = ((0,0), (0,0), S.empty)
        (_,_, final) = foldl' (move step) initial input
answer1 = part1 <$> input


type State2 = ([Pos], History)

step2 :: Direction -> State2 -> State2
step2 dir (h : rest, hist) = (h' : rest', S.insert t hist)
  where h' = moveDir h dir
        dup x = (x, x)
        (t, rest') = mapAccumL (\acc e -> dup $ catchUp acc e) h' rest


part2 :: [Command] -> Int
part2 input = S.size final
  where initial = (replicate 10 (0,0), S.empty)
        (_, final) = foldl' (move step2) initial input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
