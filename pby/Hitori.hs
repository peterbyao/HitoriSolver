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
import qualified GHC.TypeLits as unshaded

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
    deriving (Show, Eq)

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
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- Given a list of cells, generate all neighboring pairs to check for rule (2)
adjPairs :: [a] -> [(a, a)]
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
checkMultiVal :: [(Int, Int)] -> [Char] -> [Expr]
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

-- Function push adds an item to the queue
push :: a -> Queue a -> Queue a
push y (Queue xs ys) = Queue xs (y:ys)

-- Function pop removes an item from the end of the queue, returning a Maybe
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue [] xs) = pop (Queue (reverse xs) []) -- A true queue would need to reverse the second list
pop (Queue (x:xs) ys) = Just (x, Queue xs ys)

-- Get valid paths from cells
validPaths :: Array (Int, Int) Bool -> (Int, Int) -> (Int, Int) -> [Cell]
validPaths arr (x, y) (m, n) = error "hello"
    -- Corner case
    | x == 0 && y == 0 = filter (\(i, j) -> Arr ! (i, j)) [(Cell (x+1, y), Cell (x, y+1))]
    | x == 0 && y == n = filter (\(i, j) -> Arr ! (i, j)) [(Cell (x+1, y), Cell (x, y-1))]
    | x == m && y == 0 = filter (\(i, j) -> Arr ! (i, j)) [(Cell (x-1, y), Cell (x, y+1))]
    | x == m && y == n = filter (\(i, j) -> Arr ! (i, j)) [(Cell (n-1, m), Cell (n, m+1))]

    -- Top edge case
    | y == 0 = filter (\(i, j) -> Arr ! (i, j)) [(Cell ())]

    -- Bottom edge case
    | y == n

    -- Left edge case
    | x == 0

    -- Right edge case
    | x == m

    -- Middle case
    | Otherwise



-- Check if look for a path between (x0, y0) and (x1, y1)
bfs :: Queue Cell -> Cell -> Boolean
bfs q (Cell (x, y)) =
    case pop q of
        Nothing -> False
        Just Cell (x0, y0)
            | x0 == x && y0 == y -> True
            | otherwise -> bfs (push ...) (Cell (x, y))


