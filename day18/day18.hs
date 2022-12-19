{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.List as L

import qualified Data.Ix as I
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (asum)


test =  map parse [ "2,2,2"
                  , "1,2,2"
                  , "3,2,2"
                  , "2,1,2"
                  , "2,3,2"
                  , "2,2,1"
                  , "2,2,3"
                  , "2,2,4"
                  , "2,2,6"
                  , "1,2,5"
                  , "3,2,5"
                  , "2,1,5"
                  , "2,3,5"
                  ]
input = map parse . lines <$> readFile "input.txt"

type Cube = (Int, Int, Int)
type Input = [Cube]

parse :: String -> Cube
parse str = read $ "(" ++ str ++ ")"

neighbourhood (x,y,z) = [(x-1, y, z), (x+1, y, z),
                         (x, y-1, z), (x, y+1, z),
                         (x, y, z-1), (x, y, z+1)]

connected cs = connections
  where cubes = Set.fromList cs
        connections = [ (c, con) | c <- cs, let con = [ con | con <- neighbourhood c,
                                                        con `Set.member` cubes] ]
part1, part2 :: Input -> Int
part1 input = sum [ 6 - length con | (_, con) <- connected input ]
answer1 = part1 <$> input

part2 input = undefined
answer2 = part2 <$> input

main = undefined -- do
  -- inp <- input
  -- print $ part1 inp
  -- print $ part2 inp
