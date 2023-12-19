module TestModules.Lookahead (lookAheadSeq, lookAheadPar) where

{-

 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 - DPLL implementation is based on https://gist.github.com/gatlin/1755736
 - Parallel implementation is based on Berger and Sabel: 
   https://www2.ki.informatik.uni-frankfurt.de/papers/sabel/berger-sabel-13.pdf
 - Lookahead logic inspired by Marijn Heule (References listed in Final Report)

 DPLL and Parallel implentation based on Gatlin and Berger and Sabel. The lookahead
 logic was written and is (poorly) based on various papers and lecture by CMU Professor
 Marijn Heule. Here, we look at each potential literal and evaluate what the reduced
 subproblems would look like. We implement a reduction measure suggested by Huele to
 determine the number of reduced clauses in the resulting subproblems. These are added
 together to provide a measure of reduction. We pick the literal with the most reduction
 and continue on with DPLL.

-}


import Control.Parallel.Strategies
import Control.DeepSeq
import Data.Set (toList, fromList, Set, (\\))

type Threshold = Int
type Literal = Int
type Clause = [Literal]
type Assignment = [Literal]
type Formula = [Clause]

-- Find unit clauses that can be reduced once they are found.
findUnit :: Formula -> Maybe Literal
findUnit [] = Nothing
findUnit (clause:clauses)
    | length clause == 1 = Just (head clause)
    | otherwise = findUnit clauses

-- Simplify a formula with a given literal assignment
resolve :: Literal -> Formula -> Formula
resolve _ [] = []
resolve l (clause:clauses)
    | l `elem` clause = resolve l clauses
    | otherwise = filter (/= (-l)) clause : resolve l clauses


{-
12/13 Addition
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


-- Parallelized measure of reduction (faster)
reductionPar :: Formula -> [(Int, Int)]
reductionPar f = zip (force $ parMap rseq (function f) vars) vars
                        where
                            vars = toList $ getVariables f
                            s = fromList f
                            function :: Formula -> Int -> Int
                            function f' l = (length (fromList(simplify f' l) \\ s) + 
                                            length (fromList(simplify f' (-l)) \\ s))


-- Simplify a formula given a literal.
simplify :: Formula -> Literal -> Formula
simplify !f !l = [ simpClause x l | x <- f, not (elem l x) ]
    where
        simpClause c l' = Prelude.filter (/= -l') c


{-
Solver functions to be exported
-}

-- Parallel implementation with a threshold (Suggested by Berger and Sabel)
lookAheadPar :: Threshold -> Formula -> Assignment -> Assignment
lookAheadPar _ [] model = model
lookAheadPar i clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of 
            Just u -> lookAheadPar i (resolve u clauses) (u:model)
            Nothing -> 
                let dlit = chooseLookaheadLit clauses
                --let dlit = findLiteral clauses
                    positivePath = lookAheadPar (i-1) (resolve dlit clauses) (dlit:model)
                    negativePath = lookAheadPar (i-1) (resolve (-dlit) clauses) ((-dlit): model)
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

-- Serial implementation
lookAheadSeq :: Formula -> Assignment -> Assignment
lookAheadSeq [] model = model
lookAheadSeq clauses model
    | [] `elem` clauses = []
    | otherwise =
        case findUnit clauses of
            Just u -> lookAheadSeq (resolve u clauses) (u:model)
            Nothing -> 
                let dlit = chooseLookaheadLit clauses
                    positivePath = lookAheadSeq(resolve dlit clauses) (dlit:model)
                    negativePath = lookAheadSeq (resolve (-dlit) clauses) ((- dlit):model)
                in case positivePath of
                    [] -> negativePath
                    xs -> xs
