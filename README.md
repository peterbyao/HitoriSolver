# Hitori/SAT solver

A Hitori solver written in Haskell. Our implementation represents the board state as a satisfiability problem and then solves it.

## Build/Run instructions

Build the solver with

```
stack build
```

and run it with

```
stack run <path-to-file> <filetype> <solver-algorithm> <parallel>
```

Notes on the args:

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
