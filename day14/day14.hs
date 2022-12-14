{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as S
import Text.ParserCombinators.ReadP

import Control.Monad (forM_)
import Data.Ix
import qualified Data.Array as A
import Data.Array ((!))


test =  map parse [ "498,4 -> 498,6 -> 496,6"
                  , "503,4 -> 502,4 -> 502,9 -> 494,9"
                  ]
input = map parse . lines <$> readFile "input.txt"


type Pos = (Int, Int)
type Line = [Pos]
type Input = [Line]

parse :: String -> Line
parse str = res
  where [(res, _)] = readP_to_S (line <* eof) str
        int = read <$> munch1 C.isDigit
        pos = (,) <$> (int <* char ',') <*> int
        line = sepBy1 pos (string " -> ")

data Cell = Empty | Rock | Sand
  deriving (Eq, Show)

type Grid = A.Array Pos Cell

toGrid lines = A.accumArray (\ _ x -> x) Empty bnds (zip rocks $ repeat Rock)
  where expand prev (next, acc) = (prev, range (min next prev, max next prev) ++ acc)
        rocks = concatMap (\line -> concatMap (\[x,y] -> range (min x y, max x y)) $ S.divvy 2 1 line) lines
        (xs, ys) = unzip rocks
        bnds = ((minimum xs-1, 0), (maximum xs, maximum ys))

showGrid :: Grid -> IO()
showGrid grid = do
  forM_ [minY .. maxY] $ \j -> do
    forM_ [minX .. maxX] $ \i -> do
       putStr $ cell $ grid ! (i,j)
    putStrLn ""

  where ((minX, minY), (maxX, maxY)) = A.bounds grid
        cell = \case Empty -> "."; Rock -> "#" ; _ -> "o"

toRest grid = \case
  (i, j) | not $ bnds `A.inRange` (i, j+1) -> Nothing
  (i, j) | grid ! (i, j+1) == Empty -> toRest grid (i, j+1)
  (i, j) | grid ! (i-1, j+1) == Empty -> toRest grid (i-1, j+1)
  (i, j) | grid ! (i+1, j+1) == Empty -> toRest grid (i+1, j+1)
  idx -> Just idx
  where bnds = A.bounds grid

step toRest grid = fmap (\idx -> grid A.// [(idx, Sand)]) $ toRest grid (500,0)

stages toRest grid = L.unfoldr (\g -> case step toRest g of
                                   Nothing -> Nothing
                                   Just g' -> Just (g, g')) grid

part1, part2 :: Input -> Int
part1 input = length . stages toRest . toGrid $ input
answer1 = part1 <$> input

toRest2 grid = \case
  (i, j) | grid ! (500, 0) /= Empty -> Nothing
  (i, j) | grid ! (i, j+1) == Empty -> toRest2 grid (i, j+1)
  (i, j) | grid ! (i-1, j+1) == Empty -> toRest2 grid (i-1, j+1)
  (i, j) | grid ! (i+1, j+1) == Empty -> toRest2 grid (i+1, j+1)
  idx -> Just idx
  where bnds = A.bounds grid

addFloor grid = A.accumArray (\ _ x -> x) Empty bnds $ floor ++ A.assocs grid
  where ((lx,ly), (ux, uy)) = A.bounds grid
        bnds@((lx', _), (ux', uy')) = ((lx-uy-10, ly), (ux+uy+10, uy+2))
        floor = [(i, Rock) | i <- range ((lx', uy'), (ux', uy'))]

part2 input = length . stages toRest2 . addFloor . toGrid $ input
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
