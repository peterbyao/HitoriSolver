# Hitori/SAT solver

A Hitori solver written in Haskell. Our implementation represents the board state as a satisfiability problem and then solves it.

## Running from Command Line Arguments

Build the main executable:

```
stack build
```

Now you have `HitoriSolver-exe`, which recieves command line arguments and picks the solver accordingly. Use it by running:

```
stack run <path-to-file> <filetype> <solver-algorithm> <parallel>
```

Some notes on the args:

- `<filetype>` must be either `txt` or `cnf`.
  - `txt` implies a Hitori board instance, which is converted into CNF and then solved
  - `cnf` is parsed and solved directly.
- `<solver-algorithm>` must be `dpll` (David-Putnam-Loveland-Logemann), `cdcl` (conflict-driven-clause-learning), or `lookahead`.
- `<parallel>` must be either `par` or `seq`
  - Note that for now, you cannot tune the parallel parameters (depth, number of cores, etc)

For example, running this from the root directory of the project...

```
stack run boards/19x19.txt txt dpll seq
```

... will run the provided 19x19 Hitori board using the sequential DPLL solver and print the solved result.

## Algorithm-specific executables (for threadscoping)

More premade tests are in the `tests/` directory. These are how we tested our program. They run on the same CNF instance, using a different algorithm depending on the `.hs` file:

- `DPLLPar.hs`: parallel run of DPLL using a depth of 20. No lookahead logic, only naively picking the next literal.
- `CubeNConquer.hs`: parallel run of CDCL using the Cube-and-conquer approach.
- `LookaheadPar.hs`: parallel lookahead solver with depth of 20 (dpllPar with lookahead instead)
