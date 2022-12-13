{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as L
import Text.ParserCombinators.ReadP


test =  parse $ unlines [ "[1,1,3,1,1]"
                        , "[1,1,5,1,1]"
                        , ""
                        , "[[1],[2,3,4]]"
                        , "[[1],4]"
                        , ""
                        , "[9]"
                        , "[[8,7,6]]"
                        , ""
                        , "[[4,4],4,4]"
                        , "[[4,4],4,4,4]"
                        , ""
                        , "[7,7,7,7]"
                        , "[7,7,7]"
                        , ""
                        , "[]"
                        , "[3]"
                        , ""
                        , "[[[]]]"
                        , "[[]]"
                        , ""
                        , "[1,[2,[3,[4,[5,6,7]]]],8,9]"
                        , "[1,[2,[3,[4,[5,6,0]]]],8,9]"
                        ]
input = parse <$> readFile "input.txt"

data Package = Lst [Package]
             | I Int
  deriving (Eq, Show)
type Pair = (Package, Package)

type Input = [Pair]

parse :: String -> Input
parse str = [ (p1, p2) | pair <- L.splitOn "\n\n" str,
              let [p1, p2] = map parsePackage $ lines pair ]

parsePackage str = res
  where [(res, _)] = readP_to_S package str

        package = int <++ lst

        lst = between (char '[') (char ']') $ do
          Lst <$> sepBy package (char ',')

        int = I . read <$> munch1 C.isDigit

instance Ord Package where
  compare (I x) (I y) = compare x y
  compare (I x) ys@(Lst _) = compare (Lst [I x]) ys
  compare xs@(Lst _) (I y) = compare xs $ Lst [I y]
  compare (Lst []) (Lst []) = EQ
  compare (Lst []) (Lst (_ : _)) = LT
  compare (Lst (_:_)) (Lst[]) = GT
  compare (Lst (x : xs)) (Lst (y : ys)) =
    case compare x y of
      EQ -> compare (Lst xs) (Lst ys)
      x -> x

part1, part2 :: Input -> Int
part1 input = sum [ i | ((x,y), i) <- zip input [1..], x < y ]
answer1 = part1 <$> input

part2 input = product $ map (+1) indices
  where (ls, rs) = unzip input
        dps = [Lst[Lst[I 2]], Lst[Lst[I 6]]]
        allPackages = dps ++ ls ++ rs
        sorted = L.sort allPackages
        indices = L.findIndices (\p -> p `elem` dps) sorted

answer2 = part2 <$> input

main = do
  inp <- input
  print $ part1 inp
  print $ part2 inp
