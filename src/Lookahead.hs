module Lookahead (Literal, Clause, Formula, solve, getInitCube, solverToFormula) where

{-
 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 DPLL Implementation is based on https://gist.github.com/gatlin/1755736

 We modify the DPLL implementation to include lookahead logic. This will replace the "first"
-}

import Data.Maybe
import Data.Set (toList, fromList, Set, (\\))
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
                                                            pos = identifyConflict $ (SolverState (simplify f l) (l:r))
                                                            neg = identifyConflict $ (SolverState (simplify f (-l)) ((-l):r))
                                                        in
                                                            case (pos, neg) of
                                                                (Nothing, Nothing) -> []
                                                                (Nothing, Just n)  -> getCube n (i-1) (n:cubes)
                                                                (Just p, Nothing)  -> getCube p (i-1) (p:cubes)
                                                                (Just p, Just n)   ->
                                                                    getCube p (i-1) (p:cubes) ++ getCube n (i-1) (n:cubes)


--f = [[1,2,3],[4,5],[-1,-2],[-1,-3],[-1,-4],[-1,-5],[-2,-3],[-2,-4],[-2,-5],[-3,-4]]
-- Just [-1,-2,3,-4,5]

{-
identifyConflict checks if simplified formula contains any conflicts.
-}
identifyConflict :: SolverState -> Maybe SolverState
identifyConflict (SolverState f r) = case getUnit f of
                                        Nothing -> Just (SolverState f r)
                                        Just u  -> if conflict (simplify f u) then Nothing
                                                        else identifyConflict $ SolverState (simplify f u) (u:r)


-- Given a list, check to see whether or not unit clauses contain conflicts.
conflict :: Formula -> Bool
conflict f = case [x | [x] <- f] of
                []    -> False
                units -> ((length $ fromList (map abs units)) < (length $ units))


-- Take all the cubes and try to solve them with CDCL
conquerCubes :: Cube -> Maybe [Literal]
conquerCubes cube = case cube of
                        [] -> Nothing
                        -- cs -> map solveCDCL cubeFormula `using` parListChunk rseq
                        cs -> msum $ runEval $ parMap' solveCDCL cubeFormula
                                where cubeFormula = map ((filter (not . null)) . solverToFormula) cs



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
chooseLookaheadLit !f = case reductionSeq f of
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


parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' _ [] = return []
parMap' f (a:as) = do
                    b <- rpar (f a)
                    bs <- parMap' f as
                    return (b:bs)


{-
Parallel measure of reduction
-}
reductionPar :: Formula -> [(Int, Int)]
reductionPar f = zip (runEval $ parMap' (function f) vars) vars
                --zip (force $ parMap rpar (function f) vars) vars
                        where
                            vars = toList $ getVariables f
                            s = Data.Set.fromList f
                            function :: Formula -> Int -> Int
                            function f' l = (length (Data.Set.fromList(simplify f' l) \\ s) + 
                                            length (Data.Set.fromList(simplify f' (-l)) \\ s))


reductionSeq :: Formula -> [(Int, Int)]
reductionSeq f = [(length (Data.Set.fromList(simplify f l) \\ s) + 
                        length (Data.Set.fromList(simplify f (-l)) \\ s), l) 
                        | l <- vars]
                    where 
                        vars = toList $ getVariables f
                        s = Data.Set.fromList f

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

getInitCube :: [[Int]] -> Cube
getInitCube f = getCube (SolverState f []) depth []
