# Hitori/SAT solver

A Hitori solver written in Haskell. Our implementation represents the board state as a satisfiability problem and then solves it.

## Running from Command Line Arguments

Build the main executable:

```
stack build
```

Now you have `HitoriSolver-exe`, which recieves command line arguments and picks the solver accordingly. Use it by running:

```
stack run <path-to-file> <solver-algorithm> <optional-depth> --rts-options -s --rts-options -N
```

Some notes on the args:

- `<path-to-file>` must be either a `.txt` or `.cnf` file.
  - `.txt` implies a Hitori board instance, which is converted into CNF and then solved
  - `.cnf` is parsed and solved directly.
- `<solver-algorithm>` must be `dpll` (David-Putnam-Loveland-Logemann), `cdcl` (conflict-driven-clause-learning), or `lookahead`.
  - `dpllSeq` for Davis-Putnam-Logemann-Loveland (DPLL) algorithm with serial implementation.
  - `dpllPar` for DPLL algorithm with parallel implementation. NOTE: requires a `<depth>` parameter.
  - `cdclSeq` for Conflict-Driven-Clause-Learning (CDCL) algorithm with serial implementation.
  - `cubeAndConquer` for Cube-and-Conquer algorithm implementing look-ahead and serial CDCL. NOTE: requires a `<depth>` parameter.
  - `lookaheadSeq` for Look-Ahead algorithm with serial implementation. WARNING: slow.
  - `lookaheadPar` for Look-Ahead algorithm with parallel implementation. WARNING: slow. NOTE: requires a `<depth>` parameter.
- `<optional-depth>` must be supplied if using `dpllPar`, `cubeAndConquer`, or `lookAheadPar`. Depth does not have the same meaning for all algorithms!
  - For DPLL and Look-Ahead, `<optional-depth>` parameterizes the amount of depth to parallelize work, before continuing on sequentially. High numbers will run the entire algorithm in parallel. Low numbers will approach a purely sequential algorithm.
  - For cubeAndConquer, `<optional-depth>` parameterizes the amount of lookahead steps to perform, before solving each subproblem with parallel CDCL solvers. For large depths, this algorithm approaches pure look-ahead. For smaller depths, this algorithm approaches pure sequential CDCL. Our best depths for our sample files were between 0 and 5. Other sizes and difficulties of puzzles may need other depth parameters to run optimally.
- `--rts-options -N` optional flag enables all cores. You can set a specific number of cores by adding a number after `-N`. For example, to use 5 cores, run `--rts-options -N5`
- `--rts-options -s` optional flag enables the summary output

For example, running this from the root directory of the project...
```
stack run boards/19x19.txt dpllSeq --rts-options -s --rts-options -N
```
... will run the provided 19x19 Hitori board using the sequential DPLL solver and print the solved result. Note: if a depth parameter is supplied with a sequential algorithm, it is ignored.

Running this example from the root directory of the project...
```
stack run CBS_k3_n100_m411_b90_999.cnf cubeAndConquer 2 --rts-options -N --rts-options -s
```
... will run the CNF dimacs file with the Cube-and-Conquer strategy with all cores enabled, and summary output.

## Algorithm-specific executables (for threadscoping)

More premade tests are in the `tests/` directory. These are how we tested our program. They run on the same CNF instance, using a different algorithm depending on the `.hs` file:

- `DPLLPar.hs`: parallel run of DPLL using a depth of 20. No lookahead logic, only naively picking the next literal.
- `CubeNConquer.hs`: parallel run of CDCL using the Cube-and-conquer approach.
- `LookaheadPar.hs`: parallel lookahead solver with depth of 20 (dpllPar with lookahead instead)
