# Hitori/SAT solver

A Hitori solver written in Haskell. Our implementation represents the board state as a satisfiability problem and then solves it.

## Build/Run instructions

Build the solver with

```
stack build
```

and run it with

```
stack run <path-to-file> <filetype> <solver-algorithm> [spark-threshold]
```

Notes on the args:

- `<filetype>` must be either `txt` or `cnf`.
  - `txt` implies a Hitori board instance, which is converted into CNF and then solved
  - `cnf` is parsed and solved directly.
- `<solver-algorithm>` can be `dpll` (David-Putnam-Loveland-Logemann) or `cdcl` (conflict-driven-clause-learning).
- `[spark-threshold]` (optional) is a hyperparameter for DPLL that describes how many sparks the program should create before reverting to a sequential implementation.

For example, running this from the root directory of the project...

```
stack run boards/19x19.txt txt dpll 40
```

... will run the provided 19x19 Hitori board using the DPLL solver with threshold 40, and print the solved result.
