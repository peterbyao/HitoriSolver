{-
 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 DPLL Implementation is based on https://gist.github.com/gatlin/1755736

 We modify the DPLL implementation to include lookahead logic. This will replace the "first"
-}

module Lookahead (Literal, Clause, Formula, solve) where
import Data.Maybe
import Data.Set (toList, fromList, Set, (\\))
import Control.DeepSeq
import Control.Parallel.Strategies
import Control.Monad (msum)
import CDCL (solveCDCL)

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
type Cube = [SolverState]

-- unitpropagate simplifies the formula for every variable in a unit clause
unitpropagate :: SolverState -> SolverState
unitpropagate (SolverState f r) =
    case getUnit f of
        Nothing -> SolverState f r
        Just u -> unitpropagate $ SolverState (simplify f u) (u:r)

--Get cube using a lookahead depth
getCube :: SolverState -> Int -> [SolverState] -> Cube
getCube (SolverState f r) i cubes = case chooseLookaheadLit f of
                                        Nothing -> cubes
                                        Just l -> if i == 0 then cubes
                                                    else
                                                        let 
                                                            pos = unitpropagate $ (SolverState (simplify f l) (l:r))
                                                            neg = unitpropagate $ (SolverState (simplify f (-l)) ((-l):r))
                                                        in
                                                            getCube pos (i-1) (pos:cubes) ++ getCube neg (i-1) (neg:cubes)


conquerCubes :: Cube -> Maybe [Literal]
conquerCubes cs = msum $ force $ parMap rseq solveCDCL (map solverToFormula cs)



-- Pop to get the top of the records
pop :: Record -> Maybe (Int, Record)
pop [] = Nothing
pop (x:xs) = Just (x, xs)

--SolverState to formula
solverToFormula :: SolverState -> Formula
solverToFormula (SolverState f r) = case pop r of
                                        Nothing -> f
                                        Just (u, r') -> solverToFormula (SolverState ([u]:f) r')


chooseLookaheadLit :: Formula -> Maybe Literal
chooseLookaheadLit !f = case reductionPar f of
                            [] -> Nothing
                            xs -> Just (snd $ maximum xs)

--Gets a list of variables remaining in the formula
getVariables :: Formula -> Set Int
getVariables f = fromList (Prelude.map abs (concat f))

{- 
Sequential measure of reduction 

--Measures the number of reduced clauses. We want this to be as large as possible.
measureReduction :: Formula -> [(Int, Int)]
measureReduction f = zip [length (Data.Set.fromList(simplify f l) \\ s) + 
                                length (Data.Set.fromList(simplify f (-l)) \\ s) 
                                | l <- vars] vars
                                        where 
                                            vars = toList $ getVariables f
                                            s = Data.Set.fromList f
-}


{-
Parallel measure of reduction
-}
reductionPar :: Formula -> [(Int, Int)]
reductionPar f = zip (force $ parMap rseq (function f) vars) vars
                        where
                            vars = toList $ getVariables f
                            s = Data.Set.fromList f
                            function :: Formula -> Int -> Int
                            function f' l = (length (Data.Set.fromList(simplify f' l) \\ s) + 
                                            length (Data.Set.fromList(simplify f' (-l)) \\ s))


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
simplify !f !l = [ simpClause x l | x <- f, not (elem l x) ]
    where
        simpClause c l' = Prelude.filter (/= -l') c

-- | The top-level function wrapping `dpll` and hiding the library internals.
--   Accepts a list of lists of Integers, treating the outer list as a
--   conjunction and the inner lists as disjunctions.

depth :: Int
depth = 3


solve :: [[Int]] -> Maybe [Int]
solve f = conquerCubes $ getCube (SolverState f []) depth []