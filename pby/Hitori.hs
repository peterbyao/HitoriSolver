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
import Data.Set (notMember, Set)
import qualified Data.Set as Set

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
--Reference: https://www.reddit.com/r/haskell/comments/loj3x7/2dimensional_algebraic_data_type/

{-
Creating data structures for Boolean expressions
-}

-- newtype for cell which holds the coordinates of the boolean expression
newtype Cell = Cell (Int, Int)
    deriving (Show, Eq, Ord)

-- datatype for complex boolean expressions
data Expr = Var (Int, Int)
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
        Cell (x0 i) == True,
        Cell (x1, i) == True
    )
)

Similar for col i and cells (i, y0) and (i, y1)
-}

-- Takes a list of indices, and a list of values, and returns an Expr
checkMultiVal :: Eq a => [(Int, Int)] -> [a] -> [Expr]
checkMultiVal is vs = [ Not (And (Var i0) (Var i1))
                        | ((i0, i1), (v0, v1)) <- zip (allPairs is) (allPairs vs), v0 == v1]


{- 
Rule (2): In any row or col, adj cells cannot both be shaded

For adjacent cells (x0, y0) and (x1, y1):

NOT (
    AND (
        Cell (x0, y0) == True
        Cell (x1, y1) == True
    )
)

-}

-- Takes a list of indices, and returns an Expr
checkAdjShade :: [(Int, Int)] -> [Expr]
checkAdjShade is = [ Not (And (Var i0) (Var i1)) | (i0, i1) <- adjPairs is]


{- 
Rule (3) Unshaded cells must all be connected horizontally or vertically

Approach: For every cell, we will create a boolean to represent whether or not a connected
path exists to any unshaded (earlier) cell. This requires that we start with at least one
unshaded cell, which is not a problem due to rule (2). If the cell at (0,0) is shaded,
then it must mean that cell (0, 1) is unshaded. Therefore, we can guarantee that either cells
(0,0) or (0,1) will be unshaded.

To do this, we will use BFS using a queue data structure from Chris Okasaki's "Purely 
Functional Data Structures". Here, we will only need to add the push and pop functionality
to make our BFS work.

-}

-- Queue Data Structure and Functions
data Queue a = Queue [a] [a]

-- Empty queue
empty :: Queue a
empty = Queue [] []

-- Function push adds an item to the queue
push :: a -> Queue a -> Queue a
push y (Queue xs ys) = Queue xs (y:ys)

--Function takes queue and list of elements and inserts elements into queue
pushList :: [a] -> Queue a -> Queue a
pushList xs q = foldl (flip push) q xs

-- Function pop removes an item from the end of the queue, returning a Maybe
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue [] xs) = pop (Queue (reverse xs) []) -- A true queue would need to reverse the second list
pop (Queue (x:xs) ys) = Just (x, Queue xs ys)

-- newLayer flags when front of queue is empty
isNewLayer :: Queue a -> Bool
isNewLayer (Queue [] _) = True
isNewLayer (Queue _ _) = False

-- When we search a new layer, we need to bring the second list to the front Queue
searchNewLayer :: Queue a -> Queue a
searchNewLayer (Queue _ ys) = Queue ys []

-- Get valid paths from cells
-- TODO: Perhaps add smarter logic to speed up performance
validPaths :: Array (Int, Int) Bool -> (Int, Int) -> Set Cell -> [Cell]
validPaths arr (x, y) v
        -- Corner cases
        | x == 0 && y == 0 = filter validCell [Cell (x+1, y), Cell (x, y+1)]
        | x == 0 && y == n = filter validCell [Cell (x+1, y), Cell (x, y-1)]
        | x == m && y == 0 = filter validCell [Cell (x-1, y), Cell (x, y+1)]
        | x == m && y == n = filter validCell [Cell (x-1, y), Cell (x, y-1)]

        -- Edge cases
        | y == 0 = filter validCell [Cell (x-1, y), Cell (x+1, y), Cell (x, y+1)]
        | y == n = filter validCell [Cell (x-1, y), Cell (x+1, y), Cell (x, y-1)]
        | x == 0 = filter validCell [Cell (x, y-1), Cell (x, y+1), Cell (x+1, y)]
        | x == m = filter validCell [Cell (x, y-1), Cell (x, y+1), Cell (x-1, y)]

        -- Middle case
        | otherwise = filter validCell
                        [Cell (x, y-1), Cell (x, y+1), Cell (x-1, y), Cell (x+1, y)]
    where
        ((x0, y0), (x1, y1)) = bounds arr
        m = x1-x0
        n = y1-y0
        validCell (Cell (p, q)) = not (arr ! (p, q)) && notMember (Cell (p, q)) v


-- Check if look for a path between (x0, y0) and (x1, y1)
-- Queue -> Goal Cell -> Boolean Array Visited Set -> Boolean
bfs :: Queue Cell -> Cell -> Array (Int, Int) Bool -> Set Cell -> Bool
bfs q (Cell (x, y)) arr v
--    | not (arr ! (x, y)) = False
    | isNewLayer q = bfs (searchNewLayer q) (Cell (x, y)) arr v
    | otherwise = case pop q of
                    Nothing -> False
                    Just (Cell (x0, y0), q')
                        | x0 == x && y0 == y -> True
                        | otherwise -> let
                                            newV = Set.insert (Cell (x0, y0)) v
                                            newQ = pushList (validPaths arr (x0, y0) newV) q'
                                        in
                                            bfs newQ (Cell (x, y)) arr newV


isConnected :: Array (Int, Int) Bool -> (Int, Int) -> Bool
isConnected arr (x, y)
        --Last cell in matrix
        | x == m && y == n && not shaded = loop g g arr
        | x == m && y == n && shaded = True
        --End of Row
        | x == m && not shaded = loop g g arr && isConnected arr (0, y+1)
        | x == m && shaded = isConnected arr (0, y+1)
        --Next elem in row
        | not shaded = loop g g arr && isConnected arr (x+1, y)
        | otherwise = isConnected arr (x+1, y)
    where
        ((x0, y0), (x1, y1)) = bounds arr
        m = x1-x0
        n = y1-y0
        shaded = arr ! (x,y)
        g = Cell (x, y)
        loop (Cell (i, j)) goal a
            --First cell in matrix
            | i == 0 && j == 0 = bfs q goal a v
            --Edge case in matrix
            | i == 0 = bfs q goal a v || loop (Cell (m, j-1)) goal a
            --Previous element in row
            | otherwise = bfs q goal a v || loop (Cell (i-1, j)) goal a
                where
                    q = push goal empty
                    v = Set.empty


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
-}

combineBoolAnd :: [Expr] -> Expr
combineBoolAnd = foldr And (Const True)

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
    let startBoard = [[3, 1, 3], [3, 2, 2], [2, 3, 1]]
    let boardArr = toArray startBoard
    let (m, n) = getDim boardArr

    -- Get Rule (1) expressions
    let rule1 = [checkMultiVal (getRowIdx boardArr idx) (getRowVal boardArr idx)
                    | idx <- [0..n-1]] ++
                [checkMultiVal (getColIdx boardArr idx) (getColVal boardArr idx)
                    | idx <- [0..m-1]]

    -- Get Rule (2) expressions
    let rule2 = [checkAdjShade (getRowIdx boardArr idx) | idx <- [0..n-1]] ++
                [checkAdjShade (getColIdx boardArr idx) | idx <- [0..m-1]]

    -- Combine (1) and (2) to get a single expression
    let rules = combineBoolAnd (map combineBoolAnd rule1 ++ map combineBoolAnd rule2)

    {-
    TODO:
    * Brute force algorithm
        * Generate possible solution
        * Evaluate on rules
        * Build boolean matrix
        * check for connectedness
        * Terminate when rules and connectedness is true
        * Return the result
    -}

    -- Output the results        
    print (show rules)