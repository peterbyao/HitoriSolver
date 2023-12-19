module Hitori (
    formatCNF,
    toCNF,
    combineBoolAnd,
    getRule1,
    getRule2,
    getRule3,
    getDim,
    toArray,
    getShadedBool,
    Expr
    ) where
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
import Data.List ( sortBy, tails )
import Data.Ord (comparing)


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
getRule1 arr = filter (not . null) [checkMultiVal (getRowIdx arr idx) (getRowVal arr idx) | idx <- [0..n]] ++
               filter (not . null) [checkMultiVal (getColIdx arr idx) (getColVal arr idx) | idx <- [0..m]]
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


-- Combine rule 2 into a list of expressions connected by conjunctions
getRule2 :: Array (Int, Int) a -> [[Expr]]
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
Rule 3a
Pick an arbitrary vertex 𝑣0∈𝑉 and add the following clauses, over the variables 𝑥𝑣 for 𝑣∈𝑉

* 𝑥𝑣0

-}
--Pick an arbitrary unshaded cell
getRuleA :: Array (Int, Int) a -> [[Int]]
getRuleA arr = [[-1,v+1], [1,-(v+1)], [-v,-(v+1)], [v,v+1]]
    where
        (m,n) = getDim arr
        v = m * n + 1

{-
Rule 3B
⋁𝑣≠𝑣0¬𝑥𝑣

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
--CNF A or B or ((C or !D) and (D or !C)) => (A ∨ B ∨ ¬C ∨ D) ∧ (A ∨ B ∨ C ∨ ¬D)
ruleFromCell :: Array (Int, Int) a -> Cell -> Int -> [[Int]]
ruleFromCell arr (Cell (x, y)) i = [[toInt (Var (x,y)), toInt (Var (p,q)), -(toInt (Var3 (p, q, i))), toInt (Var3 (x, y, i))]
                                        | (Cell (p, q)) <- getGraphNeighbor arr (Cell (x, y))]++
                                    [[toInt (Var (x,y)), toInt (Var (p,q)), toInt (Var3 (p, q, i)), -(toInt (Var3 (x, y, i)))]
                                        | (Cell (p, q)) <- getGraphNeighbor arr (Cell (x, y))]
                                            where
                                                toInt expr = varToInt expr arr

getRuleB :: Array (Int, Int) a -> [[Int]]
getRuleB arr = concat [ruleFromCell arr (Cell (x,y)) 0 | x <- [0..m], y <- [0..n]]
                        where
                            ((x0, y0), (x1, y1)) = bounds arr
                            m = x1-x0
                            n = y1-y0


{-
Rule C
⋁𝑣≠𝑣0¬𝑥𝑣
-}
getRuleC :: Array (Int, Int) a -> [[Int]]
getRuleC arr = [[toInt (Var (x, y)), toInt (Var3 (x, y, 0))] | x <- [0..m], y <- [0..n], (x, y) /= (0,0), (x, y) /= (0,1)]
                    where
                        ((x0, y0), (x1, y1)) = bounds arr
                        m = x1-x0
                        n = y1-y0
                        toInt expr = varToInt expr arr


{-
All nodes must have a valid edge
-}
getRuleD :: Array (Int, Int) a -> [[Int]]
getRuleD arr = [varToInt (Var (x,y)) arr : neighbor (x,y) | x <- [0..m], y <- [0..n]]
                        where
                            ((x0, y0), (x1, y1)) = bounds arr
                            m = x1-x0
                            n = y1-y0
                            neighbor (x,y) = [-(varToInt (Var (p,q)) arr)| (Cell (p, q)) <- getGraphNeighbor arr (Cell (x, y))]


getRule3 :: Array (Int, Int) a -> [[Int]]
getRule3 arr = ruleA ++ ruleB ++ ruleC ++ ruleD
                    where
                        ruleA = getRuleA arr
                        ruleB = getRuleB arr
                        ruleC = getRuleC arr
                        ruleD = getRuleD arr

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
Helper functions to display solutions
* Show initial board state
* Show solved board state
* Format boolean solution

-}
getShadedBool :: [Int] -> [Int]
getShadedBool = sortBy (comparing abs)
