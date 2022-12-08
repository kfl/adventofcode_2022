{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.Array.Unboxed as A

test =  parse [ "30373"
              , "25512"
              , "65332"
              , "33549"
              , "35390"
              ]

input = parse . lines <$> readFile "input.txt"

type Trees = A.Array (Int, Int) Int

parse :: [String] -> Trees
parse input = arr
  where
    xs = map (map C.digitToInt) input
    arr = A.listArray ((1,1),(length xs, length (head xs))) $ concat xs

isVisible :: Trees -> ((Int, Int), Int) -> Bool
isVisible trees ((i,j), x) = i `elem` [li,ui] ||
                             j `elem` [lj, uj] ||
                             higher left || higher right ||
                             higher top || higher bot
  where
    ((li,lj), (ui,uj)) = A.bounds trees
    higher = all (x >)
    left = [trees A.! (i,c) | c <- A.range (lj, j-1)]
    right = [trees A.! (i,c) | c <- A.range (j+1, uj)]
    top = [trees A.! (r,j) | r <- A.range (li, i-1)]
    bot = [trees A.! (r,j) | r <- A.range (i+1, ui)]

visible trees = filter (isVisible trees) $ A.assocs trees

part1 :: Trees -> Int
part1 input = length $ visible input
answer1 = part1 <$> input

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

scenicScore :: Trees -> ((Int, Int), Int) -> Int
scenicScore trees ((i,j), x) = viewable left * viewable right * viewable top * viewable bot
  where
    ((li,lj), (ui,uj)) = A.bounds trees
    viewable = length . takeUntil (x <=)
    left = reverse [trees A.! (i,c) | c <- A.range (lj, j-1)]
    right = [trees A.! (i,c) | c <- A.range (j+1, uj)]
    top = reverse [trees A.! (r,j) | r <- A.range (li, i-1)]
    bot = [trees A.! (r,j) | r <- A.range (i+1, ui)]

scores trees = map (scenicScore trees) $ A.assocs trees

part2 :: Trees -> Int
part2 input = maximum $ scores input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
