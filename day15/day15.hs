{-# LANGUAGE LambdaCase, Strict #-}
module Main where

import qualified Data.List as L
import qualified Text.Regex.TDFA as RE
import Text.Regex.TDFA ((=~))

import qualified Data.Ix as I
import qualified Data.Set as Set
import Data.Set (Set)


test =  map parse [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
                  , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
                  , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
                  , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
                  , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
                  , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
                  , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
                  , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
                  , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
                  , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
                  , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
                  , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
                  , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
                  , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
                  ]
input = map parse . lines <$> readFile "input.txt"


type Pos = (Int, Int)
type Line = (Pos, Pos)
type Input = [Line]

parse :: String -> Line
parse str = ((sx,sy), (bx, by))
  where r = "(-?[[:digit:]]+)"
        ds = RE.getAllTextMatches $ str =~ r
        [sx, sy, bx, by] = map read ds

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (x,y) = abs(a-x) + abs(b-y)

type Sensors = Set (Pos, Int)

mkSensors :: Input -> Sensors
mkSensors lines = Set.fromList [ (s, manhattan s b) | (s, b) <- lines ]

data BBox = BBox !Pos !Pos
  deriving (Eq, Show)

instance Semigroup BBox where
  BBox (lx, ly) (ux, uy) <> (BBox (la, lb) (ua, ub)) =
    BBox (min lx la, min ly lb) (max ux ua, max uy ub)

instance Monoid BBox where
  mempty = BBox (maxBound, maxBound) (minBound, minBound)

bbox (x, y) d = BBox (x-d, y-d) (x+d,y+d)

bounds = foldMap (\(s, d) -> bbox s d)

covered sensors pos = any (\(s, d) -> manhattan s pos <= d) sensors

impossibleAt sensors y = [ (x,y) | x <- [lx .. ux], covered sensors (x,y) ]
  where BBox (lx, _) (ux, _) = bounds sensors

part1, part2 :: Input -> Int
part1 input = length impossible - possible
  where sensors = mkSensors input
        impossible = impossibleAt sensors 2000000
        possible = length $ L.nub [ b | (_, b@(_, y)) <- input, y == 2000000]
answer1 = part1 <$> input


{-
Idea: We look for candidate positions which are just one position
beyond each sensor range. This is easy to compute as the sensor ranges
are diamond shaped because we are using Manhattan distances.
-}

candidates sensors range = Set.unions cands
  where cands = map cand $ Set.toList sensors
        valid = I.inRange range
        cand ((x,y), d) =
          Set.fromList [ (i,j) | (i,j) <- zip [x-d' .. x] [y, y-1 .. y-d'] ++
                                          zip [x .. x+d'] [y-d' .. y] ++
                                          zip [x-d' .. x] [y .. y+d'] ++
                                          zip [x .. x+d'] [y+d', y-1 .. y],
                         valid i, valid j,             -- i and j must in range
                         not $ covered sensors (i,j) ] -- cannot be covered by other sensors
          where d' = d + 1

tuning (x,y) = x * 4000000 + y

part2 input = tuning pos
  where sensors = mkSensors input
        [pos] = Set.toList $ candidates sensors (0, 4000000)
answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
