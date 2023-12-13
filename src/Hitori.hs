{-# LANGUAGE BangPatterns #-}
module Hitori where
{-

 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 This is the first attempt at creating a hitori solver, viewing the puzzle as a Constraint
 Satisfiability Problem (CSP). In this file, we create functions to create a starting board
 from a list of numbers, stored in an Array. We use the array datatype, since our starting
 state does not have to be mutable. We just want to store it for easy lookup when we are
 constructing our boolean expressions.

 In our first implementation, we will use Chars instead of Ints, because on many Hitori databases,
 larger boards will use [0..9] ++ ['A', 'B', ...]. I am uncertain about the efficiency of Char
 equality testing vs Int equality testing, but we could consider Char conversion as 'A' = 10,
 'B' = 11, and so on.

 Rules of Hitori:
 (1) No row or column can have more than one occurrence of any given number.

 (2) Shaded cells cannot be adjacent, although they can be diagonal to one another.

 (3) Unshaded cells must all be connected horizontally or vertically

 Implementation:

 True == Shaded
 False == Unshaded

-}

import GHC.Arr (Array, array, (!), bounds)
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Set (toList, fromList)




--Function will take a list of lists of Chars and transform them into an Array data type
toArray :: [[a]] -> Array (Int, Int) a
toArray vss
  = array ((0,0), (w-1,h-1))
    [ ((x,y), v) | (y, vs) <- zip [0..] vss, (x, v) <- zip [0..] vs]
  where
    w = case vss of
      [] -> 0
      vs:_ -> length vs
    h = length vss

{-
Creating data structures for Boolean expressions
-}

-- newtype for cell which holds the coordinates of the boolean expression
newtype Cell = Cell (Int, Int)
    deriving (Show, Eq, Ord)

-- datatype for complex boolean expressions
data Expr = Var (Int, Int)
            | Var3 (Int, Int, Int)
            | And Expr Expr
            | Or Expr Expr
            | Not Expr
            | Const Bool
    deriving (Show, Eq)

-- Returns a row of values of an array at index idx from a given array
getRowVal :: Array (Int, Int) a -> Int -> [a]
getRowVal arr idx = [arr ! (i, idx) | i <- [0..n]]
    where ((x0, _), (x1, _)) = bounds arr
          n = x1 - x0

-- Returns a row of indices of an array at the index idx from a given array
getRowIdx :: Array (Int, Int) a -> Int -> [(Int, Int)]
getRowIdx arr idx = [(i, idx) | i <- [0..n]]
    where ((x0, _), (x1, _)) = bounds arr
          n = x1 - x0

-- Returns a column of values an array at index idx from a given array
getColVal ::  Array (Int, Int) a -> Int -> [a]
getColVal arr idx = [arr ! (idx, i) | i <- [0..n]]
    where ((_, y0), (_, y1)) = bounds arr
          n = y1 - y0

-- Returns a column of indices of an array at the index idx from a given array
getColIdx :: Array (Int, Int) a -> Int -> [(Int, Int)]
getColIdx arr idx = [(idx, i) | i <- [0..n]]
    where ((_, y0), (_, y1)) = bounds arr
          n = y1 - y0


-- Returns the dimensions of an array
getDim :: Array (Int, Int) a -> (Int, Int)
getDim arr = (m, n)
    where ((x0, y0), (x1, y1)) = bounds arr
          m = x1-x0 + 1
          n = y1-y0 + 1


-- Given a list of cells, generate all pairs to check for rule (1)
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- Given a list of cells, generate all neighboring pairs to check for rule (2)
adjPairs :: [a] -> [(a, a)]
adjPairs [] = []
adjPairs (x:xs) = zip (x:xs) xs


{-
Rule (1): In any row or col, any value cannot occur more than once

For row i and cells (x0, i) and (y0, i):

NOT (
    AND (
        Cell (x0 i) val == Cell (x1, i) val,
        Cell (x0 i) == False,
        Cell (x1, i) == False
    )
)

Similar for col i and cells (i, y0) and (i, y1)
-}

-- Takes a list of indices, and a list of values, and returns an Expr
checkMultiVal :: Eq a => [(Int, Int)] -> [a] -> [Expr]
checkMultiVal is vs = [ Not (And (Not (Var i0)) (Not (Var i1)))
                        | ((i0, i1), (v0, v1)) <- zip (allPairs is) (allPairs vs), v0 == v1]


-- Combines rule 1 into a list of expressions connected by conjunctions
getRule1 :: Eq a => Array (Int, Int) a -> [[Expr]]
getRule1 arr = filter (not . null) [checkMultiVal (getRowIdx arr idx) (getRowVal arr idx) | idx <- [0..n-1]] ++
               filter (not . null) [checkMultiVal (getColIdx arr idx) (getColVal arr idx) | idx <- [0..m-1]]
                    where
                        ((x0, y0), (x1, y1)) = bounds arr
                        m = x1-x0
                        n = y1-y0


{- 
Rule (2): In any row or col, adj cells cannot both be shaded

For adjacent cells (x0, y0) and (x1, y1):

NOT (
    AND (
        Cell (x0, y0) == True
        Cell (x1, y1) == True
    )
)

Equivalent to:

OR (
    Cell (x0, y0) == Not True
    Cell (x1, y1) == Not True
)
-}

-- Takes a list of indices, and returns an Expr
checkAdjShade :: [(Int, Int)] -> [Expr]
checkAdjShade is = [Not (And (Var i0) (Var i1)) | (i0, i1) <- adjPairs is]


-- Combines rule 2 into a list of expressions connected by conjunctions
getRule2 :: Eq a => Array (Int, Int) a -> [[Expr]]
getRule2 arr = [checkAdjShade (getRowIdx arr idx) | idx <- [0..n]] ++
               [checkAdjShade (getColIdx arr idx) | idx <- [0..m]]
                    where
                        ((x0, y0), (x1, y1)) = bounds arr
                        m = x1-x0
                        n = y1-y0


{- 
Rule (3) Unshaded cells must all be connected horizontally or vertically

To determine if the shaded cells create two or more islands of unshaded cells, we
will treat the unshaded cells as nodes of a graph. Any edge (u, v) connecting nodes
u and v will be valid if both u and v are unshaded, and they lie adjacent. For each
node v in |V| and i in {0, 1, ... n - 1} where n is the total number of cells, we will
define a new variable x_{v,i} to represent whether or not node v is reachable from
node 0 in i or fewer moves. To do this, we must have a starting node that is unshaded.
However, due to rule 2, we know that either the first cell must be unshaded, or its
adjacent cell must be unshaded. This gives us only two cases to check for.

The following boolean rules must be satisfied in a conjunction:

(A) Only the starting node is reachable from the starting node in 0 moves. All other
nodes require > 0 moves to reach.

    for all v != v_0 ==> not x_{v,0}

(B) For every vertex v and every length i in {0, 1, ..., n-2}, x_{v, i+1} being true
implies that either x_{v,i} is true or x_{u,i} is true where u is a neighbor of v--(u,v)
is a valid edge.

    for all v in V and i in {0, 1, ..., n-2}
    ==> not x_{v, i+1} or x_{v, i} or x_{u, i} for all u s.t. (u, v) in E

(C) For every vertex v, the starting node is reachable from v in (n - 1) moves or fewer

    for all v in V ==> x_{v, n-1}

-}

{-
Rule 3A: Only vertex that can reach the starting vertex in 0 moves is the starting vertex.

For the starting vertex, we already know that rule NOT (AND (0,0) (0,1)) exists.

-}

getRuleA :: Array (Int, Int) a -> [Expr]
getRuleA arr = [Not (Var3 (x, y, 0)) | x <- [0..m], y <- [0..n], (x,y) /= (0,0), (x, y) /= (0, 1)]
                    where
                        ((x0, y0), (x1, y1)) = bounds arr
                        m = x1-x0
                        n = y1-y0


{-
Rule 3B

    for all v in V and i in {0, 1, ..., n-2}
    ==> not x_{v, i+1} or x_{v, i} or x_{u, i} for all u s.t. (u, v) in E

-}
getGraphNeighbor :: Array (Int, Int) a -> Cell -> [Cell]
getGraphNeighbor arr (Cell (x, y))
        --(u, v) edge valid if 
        -- Corner cases
        | x == 0 && y == 0 = [Cell (x+1, y), Cell (x, y+1)]
        | x == 0 && y == n = [Cell (x+1, y), Cell (x, y-1)]
        | x == m && y == 0 = [Cell (x-1, y), Cell (x, y+1)]
        | x == m && y == n = [Cell (x-1, y), Cell (x, y-1)]

        -- Edge cases
        | y == 0 = [Cell (x-1, y), Cell (x+1, y), Cell (x, y+1)]
        | y == n = [Cell (x-1, y), Cell (x+1, y), Cell (x, y-1)]
        | x == 0 = [Cell (x, y-1), Cell (x, y+1), Cell (x+1, y)]
        | x == m = [Cell (x, y-1), Cell (x, y+1), Cell (x-1, y)]

        -- Middle case
        | otherwise = [Cell (x, y-1), Cell (x, y+1), Cell (x-1, y), Cell (x+1, y)]
    where
        ((x0, y0), (x1, y1)) = bounds arr
        m = x1-x0
        n = y1-y0


--Array -> Cell -> length i -> List of list of ints
ruleFromCell :: Array (Int, Int) a -> Cell -> Int -> [[[Int]]]
ruleFromCell arr (Cell (x, y)) i = [[[-(toInt (Var3 (x, y, i+1)))]], [[toInt (Var3 (x, y, i))]]] ++
                                   [[[-(toInt (Var (x,y)))], [-(toInt (Var (p,q)))], [toInt (Var3 (p, q, i))]]
                                        | (Cell (p, q)) <- getGraphNeighbor arr (Cell (x, y))]
                                            where
                                                toInt expr = varToInt expr arr


dnfToCNF :: [[[Int]]] -> [[Int]]
dnfToCNF = foldr dist [[]]

dist :: [[Int]] -> [[Int]] -> [[Int]]
dist a b = [x++y | x <- a, y <- b]

reduce :: [[Int]] -> [[Int]]
reduce = fmap (toList . fromList)

removeDup :: [[Int]] -> [[Int]]
removeDup [] = [[]]
removeDup [[]] = []
removeDup [x] = [x]
removeDup (x:xs) = x : filter (\y -> intersect x y /= x) xs

getRuleB :: Array (Int, Int) a -> [[Int]]
getRuleB arr = concat [removeDup $ reduce $ dnfToCNF $ ruleFromCell arr (Cell (x,y)) i
                        | x <- [0..m], y <- [0..n], i <- [0..((m+1)*(n+1)-2)]]
                            where
                                ((x0, y0), (x1, y1)) = bounds arr
                                m = x1-x0
                                n = y1-y0

{-
Rule 3C: All vertexes must be reachable from start node in under (n-1) steps

Not shaded => reachable
-}

getRuleC :: Array (Int, Int) a -> [Expr]
getRuleC arr = [Or (Var (x, y)) (Var3 (x, y, i)) | x <- [0..m], y <- [0..n]]
                    where
                        ((x0, y0), (x1, y1)) = bounds arr
                        m = x1-x0
                        n = y1-y0
                        i = (m + 1) * (n + 1) - 1


{-
Combine to get Rule 3
-}
isConnected :: Array (Int, Int) Int -> [[Int]]
isConnected arr = ruleStart ++ ruleA ++ ruleB ++ ruleC
    where
        (m,n) = getDim arr
        v = m * n + 1
        ruleStart = [[-1,-v], [-2,-v], [-v,-(v+1)], [v,v+1]]
        ruleA     = formatCNF (combineBoolAnd (getRuleA arr)) arr
        ruleB     = getRuleB arr
        ruleC     = formatCNF (combineBoolAnd (getRuleC arr)) arr

{-
Board Solver Functions

Writing functions to combine expressions and evaluate them. Perhaps there is a way to simplify
complex expressions to CNF so that we can use Conflict-Driven Clause Learning (CDCL) or Davis-
Putnam-Logemann-Loveland (DPLL) algorithm. Also possible to use MiniSat.
* https://hackage.haskell.org/package/minisat
* https://hackage.haskell.org/package/minisat-solver-0.1/docs/SAT-MiniSat.html

TODO:
* Functions to evaluate complex Boolean expressions
* Functions to simplify Boolean expressions to CNF
* Write SAT solver using (CDCL faster than DPLL)

Resrouces:
https://www.gibiansky.com/blog/verification/writing-a-sat-solver/index.html

-}

combineBoolAnd :: [Expr] -> Expr
combineBoolAnd [] = Const True
combineBoolAnd [x] = x
combineBoolAnd (x:xs) = And x (combineBoolAnd xs)

-- Get rid of negations by applying De Morgan's laws and removing double negations.
fixNegations :: Expr -> Expr
fixNegations expr =
  case expr of
    -- Remove double negatives
    Not (Not x) -> fixNegations x

    -- De Morgan's laws
    Not (And x y) -> Or (fixNegations $ Not x) (fixNegations $ Not y)
    Not (Or x y) -> And (fixNegations $ Not x) (fixNegations $ Not y)

    -- Deal with constants.
    Not (Const b) -> Const (not b)

    -- Recurse on sub-terms.
    Not x -> Not (fixNegations x)
    And x y -> And (fixNegations x) (fixNegations y)
    Or x y -> Or (fixNegations x) (fixNegations y)
    x -> x

-- Attempt to distribute Or over And.
distribute :: Expr -> Expr
distribute expr =
  case expr of
    -- Distribute over and in either position.
    Or x (And y z) ->
      And (Or (distribute x) (distribute y))
          (Or (distribute x) (distribute z))
    Or (And y z) x ->
      And (Or (distribute x) (distribute y))
          (Or (distribute x) (distribute z))

    -- Recurse on sub-terms.
    Or x y -> Or (distribute x) (distribute y)
    And x y -> And (distribute x) (distribute y)
    Not x -> Not (distribute x)
    x -> x

-- Convert an expression to CNF.
toCNF :: Expr -> Expr
toCNF expr =
  if updated == expr
  then expr
  else toCNF updated
    where
        updated = distribute (fixNegations expr)

-- Expression -> m dimension -> n dimension -> CNF variable number
varToInt :: Expr -> Array (Int, Int) a -> Int
varToInt expr arr =
    case expr of
        Var (x, y) -> m * x + (y + 1)
        Var3 (x, y, i) -> m * x + (y + 1) + (i + 1) * (m * n)
        _ -> error "Invalid input"
    where
        ((x0, y0), (x1, y1)) = bounds arr
        m = x1-x0+1
        n = y1-y0+1

-- Convert CNF to list of lists of Integers for use in the DPLL solver
formatCNF :: Expr -> Array (Int, Int) Int -> [[Int]]
formatCNF expr arr =
    case expr of
        And x y              -> formatCNF x arr ++ formatCNF y arr
        Or x y               -> [concat (formatCNF x arr ++ formatCNF y arr)]
        Var (x, y)           -> [[varToInt (Var (x, y)) arr]]
        Var3 (x, y, z)       -> [[varToInt (Var3 (x, y, z)) arr]]
        Not (Var (x, y))     -> [[(-1) * varToInt (Var (x, y)) arr]]
        Not (Var3 (x, y, z)) -> [[(-1) * varToInt (Var3 (x, y, z)) arr]]
        Const True           -> [[]]
        _                    -> error "Not in CNF form"



{-
DPLL Algorithm

Adapted from github user gatlin. This implementation takes in a list of lists of ints and
provides a single solution

References:
https://gist.github.com/gatlin/1755736

-}

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


{-
Helper functions to display solutions
* Show initial board state
* Show solved board state
* Format boolean solution

-}
getShadedBool :: [Int] -> [Int]
getShadedBool = sortBy (comparing abs)




{-
        startBoard = [[15, 2, 5, 16, 11, 1, 8, 16, 14, 18, 13, 20, 17, 9, 17, 10, 19, 16, 13, 3],
                      [13, 11, 3, 2, 3, 6, 2, 18, 13, 12, 14, 12, 19, 5, 4, 18, 10, 18, 9, 15],
                      [4, 10, 20, 4, 3, 8, 14, 19, 13, 9, 4, 6, 11, 3, 7, 4, 2, 12, 16, 5],
                      [1, 15, 17, 6, 18, 5, 3, 8, 5, 4, 11, 16, 10, 14, 2, 13, 20, 19, 12, 7],
                      [13, 18, 8, 4, 16, 20, 10, 10, 11, 5, 19, 17, 8, 14, 3, 9, 5, 1, 2, 17],
                      [2, 4, 16, 12, 4, 10, 10, 5, 20, 15, 9, 14, 9, 17, 17, 8, 16, 4, 4, 11],
                      [9, 8, 5, 3, 4, 14, 12, 1, 11, 13, 20, 2, 15, 11, 2, 14, 17, 8, 19, 16],
                      [14, 4, 12, 20, 2, 11, 19, 1, 10, 5, 17, 1, 2, 8, 16, 11, 15, 6, 5, 18],
                      [7, 5, 14, 3, 8, 14, 6, 12, 15, 4, 10, 9, 18, 10, 17, 20, 3, 2, 11, 11],
                      [8, 9, 4, 10, 5, 7, 11, 16, 16, 6, 13, 17, 13, 12, 9, 19, 17, 20, 8, 14],
                      [15, 16, 11, 3, 5, 3, 17, 2, 14, 14, 4, 4, 3, 10, 13, 12, 12, 19, 13, 20],
                      [17, 4, 19, 14, 17, 12, 3, 18, 8, 1, 6, 13, 15, 11, 11, 6, 9, 16, 20, 6],
                      [20, 19, 12, 7, 5, 9, 16, 4, 12, 11, 16, 18, 17, 3, 1, 17, 6, 10, 13, 8],
                      [5, 6, 10, 18, 12, 13, 4, 11, 19, 17, 3, 15, 2, 5, 8, 1, 10, 14, 9, 5],
                      [8, 10, 9, 13, 18, 2, 7, 11, 4, 3, 15, 8, 20, 16, 5, 12, 14, 8, 10, 6],
                      [4, 12, 6, 17, 13, 15, 7, 5, 1, 14, 17, 11, 17, 19, 14, 18, 7, 5, 15, 11],
                      [7, 17, 9, 5, 6, 14, 13, 9, 1, 10, 6, 14, 4, 9, 20, 1, 11, 8, 1, 19],
                      [18, 4, 1, 19, 10, 17, 19, 6, 7, 1, 9, 4, 16, 2, 11, 5, 4, 13, 7, 12],
                      [17, 13, 2, 3, 6, 15, 9, 10, 15, 20, 3, 5, 6, 18, 12, 4, 4, 7, 14, 1],
                      [1, 7, 3, 18, 20, 15, 11, 17, 6, 19, 6, 10, 12, 12, 14, 16, 3, 9, 1, 13]]
-}


main :: IO ()
main = do

--3x3 Hitori Board
    let startBoard = [[3, 1, 3],
                      [1, 2, 2],
                      [2, 3, 1]] :: [[Int]]

{- 
--4x4 Hitori Board
    let startBoard = [[2, 4, 1, 3],
                      [2, 2, 4, 1],
                      [1, 3, 4, 3],
                      [1, 1, 2, 4]] :: [[Int]]
-}

{-
-- 5x5 Hitori Board    
    let startBoard = [[1, 2, 1, 1, 3], 
                      [2, 4, 4, 5, 1], 
                      [3, 4, 5, 2, 4], 
                      [4, 3, 2, 1, 5], 
                      [1, 5, 4, 1, 4]] :: [[Int]]
-}

    let b = toArray startBoard
    let (m, n) = getDim b

    -- Get Rule (1) expressions
    let rule1 = formatCNF (toCNF (combineBoolAnd (map combineBoolAnd (getRule1 b)))) b
    let rule2 = formatCNF (toCNF (combineBoolAnd (map combineBoolAnd (getRule2 b)))) b
    let rule3 = isConnected b

    -- Combine to get a single expression
    let cnf = filter (not . null) (rule1 ++ rule2 ++ rule3)
    print (show cnf)

    let clauses = length cnf
    let variables = varToInt (Var3 (m-1, n-1, m*n-1)) b

    case solve cnf of
        Nothing -> error "UNSAT"
        Just xs -> do
            let solution = take (m*n) (getShadedBool xs)
            print (show solution)