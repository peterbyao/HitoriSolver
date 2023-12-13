module ParallelSolver (dpllSeq, dpllPar) where

{-
A Parallelized version of a DPLL Solver.
-}

import Hitori
import Control.Parallel.Strategies
import Control.DeepSeq

type Threshold = Int
type Literal = Int
type Clause = [Literal]
type Model = [Literal]
type ClauseSet = [Clause]

-- serial implementation
dpllSeq :: ClauseSet -> Model -> Model
dpllSeq [] model = model
dpllSeq clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of
            Just u -> dpllSeq (resolve u clauses) (u:model)
            Nothing -> 
                let dlit = findLiteral clauses
                    positivePath = dpllSeq(resolve dlit clauses) (dlit:model)
                    negativePath = dpllSeq (resolve (-dlit) clauses) ((- dlit):model)
                in case positivePath of
                    [] -> negativePath
                    xs -> xs

findUnit :: ClauseSet -> Maybe Literal
findUnit [] = Nothing
findUnit (clause:clauses)
    | length clause == 1 = Just (head clause)
    | otherwise = findUnit clauses


resolve :: Literal -> ClauseSet -> ClauseSet
resolve _ [] = []
resolve l (clause:clauses)
    | l `elem` clause = resolve l clauses
    | otherwise = filter (/= (-l)) clause : resolve l clauses

-- Parallel implementation with a threshold (Suggested by Berger and Sabel)
dpllPar :: Threshold -> ClauseSet -> Model -> Model
dpllPar _ [] model = model
dpllPar i clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of 
            Just u -> dpllPar i (resolve u clauses) (u:model)
            Nothing -> 
                let dlit = findLiteral clauses
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
                    [] -> negativePath
                    xs -> xs

findLiteral :: ClauseSet -> Literal
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