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

