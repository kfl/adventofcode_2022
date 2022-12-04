open Base
open Stdio

let pair_map f (x,y) = f x, f y

let parse_range input =
  input |> String.lsplit2_exn ~on:'-' |> pair_map Int.of_string

let parse line =
  line |> String.lsplit2_exn ~on:',' |> pair_map parse_range

let test = List.map ~f:parse [
  "2-4,6-8";
  "2-3,4-5";
  "5-7,7-9";
  "2-8,3-7";
  "6-6,4-6";
  "2-6,4-8";
  ]

let input() = List.map ~f:parse @@ In_channel.read_lines "input.txt"

let fully_contains (a,b) (x,y) = a <= x && y <= b

let part1 input =
  input
  |> List.filter ~f:(fun (r1, r2) -> fully_contains r1 r2 || fully_contains r2 r1)
  |> List.length

let answer1 = printf "Part1 answer: %d\n" (part1 @@ input())

let overlap ((a,b), (x,y)) =
  let lower, upper = max a x, min b y
  in  lower <= upper

let part2 input =
  input
  |> List.filter ~f:overlap
  |> List.length

let answer2 = printf "Part2 answer: %d\n" (part2 @@ input())
