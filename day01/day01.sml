val lines = String.fields (Char.contains "\n\r")

fun readLines filename =
    let val ins = TextIO.openIn filename
    in  lines (TextIO.inputAll ins)
        before TextIO.closeIn ins
    end

infix |>
fun x |> f = f x

fun groups [] [] acc = acc
  | groups [] curr acc = curr :: acc
  | groups ("" :: rest) curr acc = groups rest [] (curr :: acc)
  | groups (cal :: rest) curr acc = groups rest ((cal |> Int.fromString |> valOf) :: curr) acc

fun sum xs = foldl (op+) 0 xs
fun maximum (x :: xs) = foldl Int.max x xs

fun parse lines = groups lines [] []

fun input() = readLines "input.txt" |> parse

val test = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n"
               |> lines
               |> parse


fun report part fmt ans =
    ( print part; print ": "
    ; fmt ans |> print
    ; print "\n")

fun part1 input = map sum input |> maximum
val answer1 = input() |> part1 |> report "Part 1" Int.toString

fun part2 input = map sum input
                      |> Listsort.sort Int.compare
                      |> rev
                      |> (fn xs => List.take (xs, 3))
                      |> sum
val answer2 = input() |> part2 |> report "Part 2" Int.toString
