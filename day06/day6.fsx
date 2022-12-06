
let input() =
    System.IO.File.ReadAllText "input.txt"

let tests = [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
              "bvwbjplbgvbhsrlpgdmjqwftvncz"
              "nppdvjthqldpwncqszvftbrmjlhg"
              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
              "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
            ]

let allDistinct xs =
    (xs |> Seq.distinct |> Seq.length) = (xs |> Seq.length)

let findMarkers size line =
    line
    |> Seq.windowed size
    |> Seq.mapi (fun i w -> i, w)
    |> Seq.filter (fun (_, w) -> allDistinct w)

let firstMarkerOfSize size input =
    input
    |> findMarkers size
    |> Seq.head
    |> fun(i, _) -> i + size

let part1 input =
    firstMarkerOfSize 4 input

let answer1 =
    printfn "Part 1 answer: %d" (input() |> part1)

let part2 input =
    firstMarkerOfSize 14 input

let answer2 =
    printfn "Part 2 answer: %d" (input() |> part2)
