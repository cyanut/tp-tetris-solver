# tp-tetris-solver
A solver for the tetris puzzles in Talos Principle, using an SMT solver with Haskell.

# Dependency
- GHC
- [Data.SBV](http://github.com/LeventErkok/sbv)
- A SMT solver supported by Data.SBV


# Usage
```
ghc solver.hs -threaded -o solver
solver h w blocks [+RTS -N<number of cores>]
```
where:
`h`: height of the board

`w`: width of the board

`blocks`: string of I,O,T,S,Z,J,L, representing all the tetris blocks listed
below

```
Block 'I': ["xxxx"]

Block 'O': ["xx",
            "xx"]

Block 'T': ["xxx",
            " x "]

Block 'S': [" xx",
            "xx "]

Block 'Z': ["xx ",
            " xx"]

Block 'J': [" x",
            " x",
            "xx"]

Block 'L': ["x ",
            "x ",
            "xx"]
```

output: the solution where each block is represented in the order of `a` to `z`

# Parallelism
To increase the chance that the solver have a good starting point to
a solution, a number of solver can be initiated to run parallel. This
significantly shortens the time to run. 

Number of the parallel solver can be controlled by setting the Haskell runtime system with `+RTS -N<number of threads>` (see the second example below)

# Color
Pipe the output to the `colorize` script for color. For example:
```
solver 4 4 TTLZ | bash colorize
```

# Example:
```
$ solver 4 4 TTLZ
d d c c
a d d c
a a b c
a b b b
```

Solve difficult board with 4 parallel threads (on an i7-870, with z3 SMT solver)
```
$ time ./solver 6 8 TTILLJJJOOZZ +RTS -N4
f f f g g e e e
k k f g c c e l
i k k g c c l l
i j j j j h h l
i i a d d b h h
a a a d d b b b

real	0m15.317s
user	1m1.153s
sys	0m0.163s
