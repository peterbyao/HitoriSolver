module DPLL (solve) where
{-
DPLL Algorithm

Adapted from github user gatlin. This implementation takes in a list of lists of ints and
provides a single solution

References:
https://gist.github.com/gatlin/1755736

-}

import Data.Maybe

type Literal = Int
type Clause  = [Literal]
type Formula = [Clause]
type Record  = [Literal]

-- | The state of a solver at any given time is a subset of the original
--   formula and a record of assignments; that is, a list of the literals
--   considered to be true.
data SolverState = SolverState { formula :: !Formula
                               , record  :: !Record
                               } deriving (Show)

-- | The core algorithm, a simple back-tracking search with unitpropagation.
dpll :: SolverState -> Maybe Record
dpll s
    | null f = return r
    | otherwise = do
        l  <- chooseLiteral f
        case dpll (SolverState (simplify f l) (l:r)) of
            Just record -> return record
            Nothing -> dpll $! SolverState (simplify f (-l)) ((-l):r)
    where
        s' = unitpropagate s
        f = formula s'
        r = record s'

-- | unitpropagate simplifies the formula for every variable in a unit clause
--   (that is, a clause with only one unit).
unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
    case getUnit f of
        Nothing -> SolverState f r
        Just u -> unitpropagate $ SolverState (simplify f u) (u:r)

-- | Returns a `Just Literal` or Nothing if the formula has no literals left.
--   Since the argument was checked to see if it was null in a previous step,
--   failure to find a literal means the formula only contains empty clauses,
--   implying the problem is unsatisfiable and `dpll` will backtrack.
chooseLiteral :: Formula -> Maybe Literal
chooseLiteral !f = listToMaybe . concat $! f

-- | If a unit clause (singleton list) exists in the formula, return the
--   literal inside it, or Nothing.
getUnit :: Formula -> Maybe Literal
getUnit !xs = listToMaybe [ x | [x] <- xs ]

-- | Simplifying a formula `f` wrt a literal `l` means, for every clause in
--   which `-l` is a member remove `-l`, and remove every clause from f which
--   contains `l`.
--
--   Reasoning: a disjunction with a false value does not need to
--   consider that value, and a disjunction with a true value is trivially
--   satisfied.
simplify :: Formula -> Literal -> Formula
simplify !f !l = [ simpClause x l | x <- f, l `notElem` x ]
    where
        simpClause c l' = filter (/= -l') c

-- | The top-level function wrapping `dpll` and hiding the library internals.
--   Accepts a list of lists of Integers, treating the outer list as a
--   conjunction and the inner lists as disjunctions.
solve :: [[Int]] -> Maybe [Int]
solve = dpll . flip SolverState []
