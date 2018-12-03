# Advent of Code

This repository contains my implementation of the [Advent of
Code](https://adventofcode.com/) puzzles, using Common Lisp. It's not that
I'm so proud that I want to share them, it's more like I'm having fun and I
might learn new tricks by exposing this code.

The intend of my playing of this game is to have fun and do the puzzles
quickly, mind you. I'm not going to extend any serious thoughs about
programming best practices here, so it will mostly show my own reflexes and
habits. Otherwise it's not fun anymore, is it?

## 2018

I prepared some tooling to output a summary of my puzzles timing and results
here, for interested readers having a quick overview. My intend is to keep
that up to data running it again each day.

I'm using [Clozure Common Lisp](https://ccl.clozure.com) Version 1.12-dev
DarwinX8664 for those hacks, it's known that SBCL might give a boost to the
results, or in other situations maybe ECL or clips would be even faster.
Should I care? well I certainly don't.

~~~
PARSER> (advent/2018:summary)
Day 1: Chronal Calibration
  Puzzle 1: sum of frequency changes
    3.958ms 520
  Puzzle 2: first frequency read twice
  699.040ms 394

Day 2: Inventory Management System
  Puzzle 1: checksum of box ids
    4.857ms 5000
  Puzzle 2: common letters in box ids one letter apart
    9.060ms ymdrchgpvwfloluktajxijsqmb

Day 3: No Matter How You Slice It
  Puzzle 1: find squares claimed more than once
  819.373ms 101565
  Puzzle 2: find the only claim asked of only once
  739.283ms 656
~~~
