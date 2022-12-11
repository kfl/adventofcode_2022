(* open Base *)
(* open Stdio *)

type worry = int
type monkey_id = int

type monkey = { mutable items : worry list;
                oper : worry -> worry;
                test : worry * monkey_id * monkey_id
              }

type monkeys = monkey array

let testing (n, pos, neg) w = if w mod n = 0 then pos else neg

let example() : monkeys =
  [| { items = [79; 98];
       oper = (fun old -> old * 19);
       test = 23, 2, 3
     };
     { items = [54; 65; 75; 74];
       oper = (fun old -> old + 6);
       test = 19, 2, 0
     };
     { items = [79; 60; 97];
       oper = (fun old -> old * old);
       test = 13, 1, 3
     };
     { items = [74];
       oper = (fun old -> old + 3);
       test = 17, 0, 1
     }
  |]

let input() : monkeys =
  [| { items = [50; 70; 89; 75; 66; 66];
       oper = (fun old -> old * 5);
       test = 2, 2, 1
     };
     {items = [85];
      oper = (fun old -> old * old);
      test = 7, 3, 6
     };
     {items = [66; 51; 71; 76; 58; 55; 58; 60];
      oper = (fun old -> old + 1);
      test = 13, 1, 3
     };
     {items = [79; 52; 55; 51];
      oper = (fun old -> old + 6);
      test = 3, 6, 4
     };
     {items = [69; 92];
      oper = (fun old -> old * 17);
      test = 19, 7, 5
     };
     {items = [71; 76; 73; 98; 67; 79; 99];
      oper = (fun old -> old + 8);
      test = 5, 0, 2
     };
     {items = [82; 76; 69; 69; 57];
      oper = (fun old -> old + 7);
      test = 11, 7, 4
     };
     {items = [65; 79; 86];
      oper = (fun old -> old + 5);
      test = 17, 5, 0
     }
  |]

let part1 monkeys =
  let counts = Array.map (fun _ -> 0) monkeys in
  let turn i monkey =
    Array.set counts i (counts.(i) + List.length monkey.items);
    List.iter (fun w -> let w' = monkey.oper w / 3 in
                        let i = testing monkey.test w'
                        in  monkeys.(i).items <- w' :: monkeys.(i).items)
      monkey.items;
    monkey.items <- [] in

  for _ = 1 to 20 do
    Array.iteri turn monkeys
  done;
  Array.sort (fun x y -> compare y x) counts;
  counts.(0) * counts.(1)

let answer1 = Printf.printf "Part1 answer: %d\n" (part1 @@ input())

let part2 monkeys =
  let counts = Array.map (fun _ -> 0) monkeys in
  let field = Array.fold_left (fun acc { test = n, _ , _; _} -> acc * n) 1 monkeys in
  let turn i monkey =
    Array.set counts i (counts.(i) + List.length monkey.items);
    List.iter (fun w -> let w' = monkey.oper w mod field in
                        let i = testing monkey.test w'
                        in  monkeys.(i).items <- w' :: monkeys.(i).items)
      monkey.items;
    monkey.items <- [] in

  for _ = 1 to 10_000 do
    Array.iteri turn monkeys
  done;
  Array.sort (fun x y -> compare y x) counts;
  counts.(0) * counts.(1)

let answer2 = Printf.printf "Part2 answer: %d\n" (part2 @@ input())
