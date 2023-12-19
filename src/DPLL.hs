module DPLL (dpllSeq, dpllPar) where

{-

 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 DPLL implementation is based on https://gist.github.com/gatlin/1755736
 Parallel implementation is based on Berger and Sabel: 
 https://www2.ki.informatik.uni-frankfurt.de/papers/sabel/berger-sabel-13.pdf

-}

import Control.Parallel.Strategies (rpar, runEval)

type Threshold = Int
type Literal = Int
type Clause = [Literal]
type Assignment = [Literal]
type Formula = [Clause] -- [[Int]]

-- Unit clauses can be reduced once they are found.
findUnit :: Formula -> Maybe Literal
findUnit [] = Nothing
findUnit (clause:clauses)
    | length clause == 1 = Just (head clause)
    | otherwise = findUnit clauses


-- Function to reduce formulas with a truth value assigned to a literal
resolve :: Literal -> Formula -> Formula
resolve _ [] = []
resolve l (clause:clauses)
    | l `elem` clause = resolve l clauses
    | otherwise = filter (/= (-l)) clause : resolve l clauses


-- Given a formula, look at each clause until we find an unassigned literal
findLiteral :: Formula -> Literal
findLiteral [] = error "findLiteral: Empty clause set"
findLiteral (clause:clauses) =
    case findUnassignedLiteral clause of
        Just l -> l
        Nothing -> findLiteral clauses

-- Given a clause, find the first unassigned literal
findUnassignedLiteral :: Clause -> Maybe Literal
findUnassignedLiteral [] = Nothing
findUnassignedLiteral (l:ls)
    | abs l `notElem` map abs ls = Just l
    | otherwise = findUnassignedLiteral ls


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
                --let dlit = chooseLookaheadLit clauses
                let dlit = findLiteral clauses 
                    positivePath = dpllPar (i-1) (resolve dlit clauses) (dlit:model)
                    negativePath = dpllPar (i-1) (resolve (-dlit) clauses) ((-dlit): model)
                in if i > 0 then
                    runEval $ do 
                        x <- rpar negativePath
                        return (case positivePath of
                            [] -> x
                            xs -> xs)
                else case positivePath of
                    -- (if we are past the threshold, then we shouldn't parallelize)
                    [] -> negativePath
                    xs -> xs


-- DPLL serial implementation
dpllSeq :: Formula -> Assignment -> Assignment
dpllSeq [] model = model
dpllSeq clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of
            Just u -> dpllSeq (resolve u clauses) (u:model)
            Nothing -> 
                --let dlit = chooseLookaheadLit clauses
                let dlit = findLiteral clauses 
                    positivePath = dpllSeq (resolve dlit clauses) (dlit:model)
                    negativePath = dpllSeq (resolve (-dlit) clauses) ((- dlit):model)
                in case positivePath of
                    [] -> negativePath
                    xs -> xs


