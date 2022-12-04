Notes and Reflections from AoC 2022
===================================

**Spoiler Warning: The following contains spoilers for the AoC tasks,
so stop reading if you haven't solved the tasks yet.**


Day 1: Calorie Counting
-----------------------

Used SML.

Was sick, so the ambition of keeping up with daily task was the real
challenge.


Day 2: Rock Paper Scissors
--------------------------

Used Haskell. Still sick. Ended up with a write-once-no-testing solution.


Day 3: Rucksack Reorganization
------------------------------

Used Rust. Still sick. Rust fitted fairly nice for the problem. 

Main snag I encountered was that `chunks` only works on slices not
iterators. However, it seem that `array_chuck` is in nightly. Or I
could have used a crate like `itertools`.


Day 4: Camp Cleanup
-------------------

Used OCaml. Straightforward puzzle about some interval logic, looking
forward to the later date puzzle involving intervals.

Used the day to brush up on modern OCaml tooling. Apparently the 2nd
edition of [Real World OCaml](https://dev.realworldocaml.org/) is
based on the `Base` and `Stdio` packages. Indeed `Base` and `Stdio`
come with some nifty addition useful for the quick-and-dirty parsing
that's often used in AoC. Thus, I enjoyed not having to write
`read_lines` and `lsplit2` for the umpteen time. Alas, the price is
that "standard" modules, such as `List`, now have an interface using
labels, and my old-school ML aesthetics balk at that. Also, while I
generally agrees with the idea that build tools should be opinionated,
it annoys me when those opinions disagrees with my opinions. Hence I
had to teach `dune` that warnings are **not** errors.
