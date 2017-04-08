# tp-tetris-solver
A solver for the tetris puzzles in Talos Principle, using an SMT solver with Haskell.

# Dependency
Haskell
[Data.SBV](http://github.com/LeventErkok/sbv)
A SMT solver supported by Data.SBV


# Usage
```
ghc solver.hs -o solver
solver h w blocks
```
where
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

# Example:
```
$ solver 4 4 TTLZ
d d c c
a d d c
a a b c
a b b b
```

### Difficult board may take a while (on a i7-870)
```
$ time ./solver 6 8 TTILLJJJOOZZ
j j h h b f f f
j j h b b i i f
l l h e b i i c
d l l e g g g c
d k k e e a g c
d d k k a a a c

real	11m5.480s
user	11m5.357s
sys	    0m0.130s
```

