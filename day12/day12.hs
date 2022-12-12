{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as Map
import qualified Data.Set as S
import Data.Set (Set)

import qualified Data.Array.IArray as I
import qualified Data.Traversable as T
import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))

test =  parse [ "Sabqponm"
              , "abcryxxl"
              , "accszExk"
              , "acctuvwj"
              , "abdefghi"
              ]

input = parse . lines <$> readFile "input.txt"

type Pos = (Int, Int)
type Grid = A.Array Pos Int
type Input = (Grid, Pos, Pos)

parse :: [String] -> Input
parse lines = (A.array bounds processed, start, end)
  where bounds = ((0,0), (length lines - 1, length (head lines) - 1))
        raw = A.assocs $ (A.listArray bounds $ concat lines :: A.UArray Pos Char)

        process (_, e) (i, 'S') = ((i, e), (i, C.ord 'a'))
        process (s, e) (i, 'E') = ((s, i), (i, C.ord 'z'))
        process acc (i, c) = (acc, (i, C.ord c))

        ((start, end), processed) = T.mapAccumL process ((0,0), (0,0)) raw


x `less` may = maybe True (x <) may

neighbours grid (i, j) = [ idx | idx <- [(i+1, j), (i-1,j), (i, j+1), (i, j-1)]
                               , A.bounds grid `A.inRange` idx ]

dijkstra :: Grid -> Pos -> Pos -> Maybe Int
dijkstra grid start goal = loop startFrontier initPathCost
  where
    index idx = A.bounds grid `A.index` idx
    m !? idx = Map.lookup (index idx) m

    startFrontier = S.singleton(0, start)
    initPathCost = Map.singleton (index start) 0

    loop frontier pathCost =
      case S.minView frontier of
        Nothing -> Nothing
        Just((c,s), frontier')
          | s == goal -> Just c
          | otherwise -> loop frontier'' pathCost'
          where
            relevant = [ (cc, n) | n <- neighbours grid s, grid ! n <= grid ! s + 1,
                         let cc = c + 1, cc `less` (pathCost !? n) ]
            frontier'' = frontier' `S.union` S.fromList relevant
            pathCost' = Map.fromList [(index n, cc) | (cc, n) <- relevant] `Map.union` pathCost


part1, part2 :: Input -> Int
part1 (grid, start, goal) = pathLength
  where Just pathLength = dijkstra grid start goal
answer1 = part1 <$> input

startPos :: Grid -> [Pos]
startPos grid = [ i | (i, c) <- A.assocs grid, c == C.ord 'a']

part2 (grid, _, goal) = minimum pathLengths
  where pathLengths = [ pl | s <- startPos grid, Just pl <- [dijkstra grid s goal] ]
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
