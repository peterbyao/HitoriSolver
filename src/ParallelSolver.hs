module ParallelSolver (dpllSeq, dpllPar) where

{-
A Parallelized version of a DPLL Solver.
-}
--import Hitori
import Control.Parallel.Strategies ( parMap, rpar, rseq, runEval )
import Control.DeepSeq ( force )
import Data.Set (toList, fromList, Set, (\\))

type Threshold = Int
type Literal = Int
type Clause = [Literal]
type Assignment = [Literal]
type Formula = [Clause]

-- serial implementation
dpllSeq :: Formula -> Assignment -> Assignment
dpllSeq [] model = model
dpllSeq clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of
            Just u -> dpllSeq (resolve u clauses) (u:model)
            Nothing -> 
                let dlit = chooseLookaheadLit clauses
                    positivePath = dpllSeq(resolve dlit clauses) (dlit:model)
                    negativePath = dpllSeq (resolve (-dlit) clauses) ((- dlit):model)
                in case positivePath of
                    [] -> negativePath
                    xs -> xs

findUnit :: Formula -> Maybe Literal
findUnit [] = Nothing
findUnit (clause:clauses)
    | length clause == 1 = Just (head clause)
    | otherwise = findUnit clauses


resolve :: Literal -> Formula -> Formula
resolve _ [] = []
resolve l (clause:clauses)
    | l `elem` clause = resolve l clauses
    | otherwise = filter (/= (-l)) clause : resolve l clauses

{- 
Parallel implementation with a threshold (Suggested by Berger and Sabel). 
Past the threshold/depth,the sequential implementation is faster
-}
dpllPar :: Threshold -> Formula -> Assignment -> Assignment
dpllPar _ [] model = model
dpllPar i clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of 
            Just u -> dpllPar i (resolve u clauses) (u:model)
            Nothing -> 
                let dlit = chooseLookaheadLit clauses
                --let dlit = findLiteral clauses
                    positivePath = dpllPar (i-1) (resolve dlit clauses) (dlit:model)
                    negativePath = dpllPar (i-1) (resolve (-dlit) clauses) ((-dlit): model)
                in if i > 0 then
                    -- parallelize
                    runEval $ do 
                        x <- rpar negativePath
                        return (case positivePath of
                            [] -> x
                            xs -> xs)
                else case positivePath of
                    -- (if we are past the threshold, then we shouldn't parallelize)
                    [] -> negativePath
                    xs -> xs

findLiteral :: Formula -> Literal
findLiteral [] = error "findLiteral: Empty clause set"
findLiteral (clause:clauses) =
    case findUnassignedLiteral clause of
        Just l -> l
        Nothing -> findLiteral clauses

findUnassignedLiteral :: Clause -> Maybe Literal
findUnassignedLiteral [] = Nothing
findUnassignedLiteral (l:ls)
    | abs l `notElem` (map abs ls) = Just l
    | otherwise = findUnassignedLiteral ls


{-
12/13 Addition (PBY)
Adding logic for lookahead function which picks the literal that causes the 
greatest reduction in problem size (measured by number of reduced clauses).
-}

-- Finding the next unassigned literal
chooseLookaheadLit :: Formula -> Literal
chooseLookaheadLit !f = case reductionPar f of
                            xs -> snd $ maximum xs

--Gets a list of variables remaining in the formula
getVariables :: Formula -> Set Int
getVariables f = fromList (Prelude.map abs (concat f))

{- 
Parallelized measure of reduction (faster)
-}
reductionPar :: Formula -> [(Int, Int)]
reductionPar f = zip (force $ parMap rseq (function f) vars) vars
                        where
                            vars = toList $ getVariables f
                            s = fromList f
                            function :: Formula -> Int -> Int
                            function f' l = (length (fromList(simplify f' l) \\ s) + 
                                            length (fromList(simplify f' (-l)) \\ s))

-- Simplify a formula given a literal
simplify :: Formula -> Literal -> Formula
simplify !f !l = [ simpClause x l | x <- f, not (elem l x) ]
    where
        simpClause c l' = Prelude.filter (/= -l') c