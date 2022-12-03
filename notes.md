Notes and Reflections from AoC 2022
===================================

**Spoiler Warning: The following contains spoilers for the AoC tasks,
so stop reading if you haven't solved the tasks yet.**


Day 1
-----

Used SML.

Was sick, so the ambition of keeping up with daily task was the real
challenge.


Day 2
-----

Used Haskell. Still sick. Ended up with a write-once-no-testing solution.


Day 3
-----

Used Rust. Still sick. Rust fitted fairly nice for the problem. 

Main snag I encountered was that `chunks` only works on slices not
iterators. However, it seem that `array_chuck` is in nightly. Or I
could have used a crate like `itertools`.
