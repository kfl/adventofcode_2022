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


Day 5: Supply Stacks
--------------------

Pre-code analysis: The hardest part seems to be the parsing, if it has
to be fully general. Fortunately we have access to the only relevant
input, and can derive some simplifying assumptions: crate names are a
single upper case letter; the input comes in two parts, initial
configuration and moves, separated by a blank line; the configuration
has trailing whitespace to match all stacks.

Used Python. Python's slicing was a nice fit for the janky parsing,
and for implementing `multimove` for part 2. Likewise, `List` was an
adequate stack. I started by giving type annotations, but after the
parsing part I didn't feel the need and stopped.


Day 6: Tuning Trouble
---------------------

Used F#. Use a window to find the markers, when all elements in a
window are distinct we have a maker. My code is a bit too general as
it find all markers, not just the first one. However, I using `Seq` so
it should be computed in a lazy fashion.


Day 7: No Space Left On Device
------------------------------

Used C++. Interesting parsing where based on the commands `cd` and
`ls`. Not a problem I would normally use C++ for, but it seemed to
make the problem more interesting, and it has been a while since I
used modern C++ in anger.


Day 8: Treetop Tree House
-------------------------

Used Haskell. Had to write a `takeUntil` function for part 2.


Day 9: Rope Bridge
------------------

Used Haskell.


Day 10: Cathode-Ray Tube
------------------------

Used C. Against one of my soft AoC principles, I ended with a solution
that solved both parts at the same time (and even prints the solution
to part 2 first). My solution for solving part 2 is basically the same
as for part 1, but with a different `check_cycle` function. Alas, the
easiest way forward was to add three lines to `check_cycle` and be
done with it.


Day 11: Monkey in the Middle
----------------------------

Pre-code analysis: Parsing looks like it could be end up being rather
interesting: what is the grammar for monkey operations, how deep
expressions are allowed? What tests are allowed, only `divisible`?
Alas, `input.txt` is so small that its not worth writing a parser,
sadness 😞.

Used OCaml, plain without any libraries. Ended up with some fairly
imperative code, but that felt like the natural paradigm for the day's
task. The specification for task 2 was a bit vague and nasty at first
glance, bit my first idea of using a the numbers from the `divisible`
tests to form a modulo field worked out. The `turn` code from part 1
and 2 could be refactored out and shared by the two parts.


Day 12: Hill Climbing Algorithm
-------------------------------

Pre-code analysis: Looks like a straightforward BFS (or
Dijkstra/A-star) problem for part 1.

Used Haskell. Implemented Dijkstra's algorithm for the path search in
part 1. Part 2 is a simple generalisation of the solution for part 1,
instead of a single starting position we have many starting positions
(not all have solutions).


Day 13: Distress Signal
-----------------------

Used Haskell. A task well-suited for Haskell: parser combinators for
input parsing, and the task for part 1 was to make a custom instance
of the `Ord` type-class. For part 2, when you have the custom `Ord`
instance, then the standard library has all the needed utility
functions (such as `sort`) for solving the task in a straightforward
manner.


Day 14: Regolith Reservoir
--------------------------

Used Haskell, but should just have used a language with mutable
arrays. Part 2 takes several seconds to find the answer.
