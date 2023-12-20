module CubeAndConquer (solveCube, getInitCube) where

{-
 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 DPLL Implementation is based on https://gist.github.com/gatlin/1755736
 CDCL Implementation is based on https://github.com/tamionv/HaskSat/blob/master/README.md
 Lookahead logic inspired by Marijn Heule (References listed in Final Report)

 We modify the DPLL implementation to include lookahead logic. Finally, we add import CDCL and
 use it to solve a cube of reduced sub-problems.
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


-- The state of a solver at any given time is a subset of the original formula and a record of assignments
data SolverState = SolverState { formula :: !Formula
                               , record  :: !Record
                               } deriving (Show)
type Cube = [SolverState]

-- DList datatype is introduced to flatten the implication graph
newtype DList a = DList ([a] -> [a])

instance Semigroup (DList a) where
    DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
    mempty = DList id

singleton :: a -> DList a
singleton a = DList (a:)

dListToList :: DList a -> [a]
dListToList (DList f) = f []

--Get cube using a lookahead depth
getCube :: SolverState -> Int-> DList SolverState
getCube (SolverState f r) i = case chooseLookaheadLit f of
                                        Nothing -> mempty
                                        Just l -> if i == 0 then 
                                                        case (pos, neg) of
                                                            (Nothing, Nothing) -> mempty
                                                            (Nothing, Just n)  -> singleton n
                                                            (Just p, Nothing)  -> singleton p
                                                            (Just p, Just n)   -> singleton p <> singleton n
                                                    else

                                                        case (pos, neg) of
                                                            (Nothing, Nothing) -> mempty
                                                            (Nothing, Just n)  -> getCube n (i-1)
                                                            (Just p, Nothing)  -> getCube p (i-1)
                                                            (Just p, Just n)   -> getCube p (i-1) <> getCube n (i-1)

                                                    where 
                                                        pos = identifyConflict $ (SolverState (simplify f l) (l:r))
                                                        neg = identifyConflict $ (SolverState (simplify f (-l)) ((-l):r))


-- Take all the cubes and try to solve them with CDCL
conquerCubes :: Cube -> Maybe [Literal]
conquerCubes cube = case cube of
                        [] -> Nothing
                        cs -> msum $ runEval $ parMap' solveCDCL cubeFormula
                                where cubeFormula = map ((filter (not . null)) . solverToFormula) cs



{-
identifyConflict checks if simplified formula contains any conflicts. The original function was
unitpropagate, which simplified the formula but did not check for conflicts.
-}

-- Function takes a solverstate and returns Nothing if a conflict is detected.
identifyConflict :: SolverState -> Maybe SolverState
identifyConflict (SolverState f r) = if conflict f then Just (SolverState f r)
                                        else case getUnit f of
                                            Nothing -> Nothing
                                            Just u  -> identifyConflict $ SolverState (simplify f u) (u:r)


-- Given a list, check to see whether or not unit clauses contain conflicts.
conflict :: Formula -> Bool
conflict f = case [x | [x] <- f'] of
                    []    -> True
                    units -> ((length $ fromList (map abs units)) < (length $ units))
                where f' = (toList . fromList) f
            


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


parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' _ [] = return []
parMap' f (a:as) = do
                    b <- rpar (f a)
                    bs <- parMap' f as
                    return (b:bs)



--Parallel measure of reduction
reductionPar :: Formula -> [(Int, Int)]
reductionPar f = zip (runEval $ parMap' (function f) vars) vars
                --zip (force $ parMap rpar (function f) vars) vars
                        where
                            vars = toList $ getVariables f
                            s = Data.Set.fromList f
                            function :: Formula -> Int -> Int
                            function f' l = (length (Data.Set.fromList(simplify f' l) \\ s) + 
                                            length (Data.Set.fromList(simplify f' (-l)) \\ s))


{-
reductionSeq :: Formula -> [(Int, Int)]
reductionSeq f = [(length (Data.Set.fromList(simplify f l) \\ s) + 
                        length (Data.Set.fromList(simplify f (-l)) \\ s), l) 
                        | l <- vars]
                    where 
                        vars = toList $ getVariables f
                        s = Data.Set.fromList f
-}

-- If a unit clause (singleton list) exists in the formula, return the literal inside it, or Nothing.
getUnit :: Formula -> Maybe Literal
getUnit !xs = listToMaybe [ x | [x] <- xs ]

-- Simplifying a formula wrt a literal means, for every clause in which `-l` is a member remove `-l`, 
-- and remove every clause from f which contains `l`.
simplify :: Formula -> Literal -> Formula
simplify !f !l = [ simpClause x l | x <- f, not (elem l x) ]
    where
        simpClause c l' = Prelude.filter (/= -l') c


-- Solve a formula with the Cube and Conquer method
solveCube :: [[Int]] -> Int -> Maybe [Int]
solveCube f depth = conquerCubes $ dListToList $ (getCube (SolverState f []) depth)

-- Exported function to display the cube
getInitCube :: [[Int]] -> Int -> Cube
getInitCube f depth = dListToList $ (getCube (SolverState f []) depth)
