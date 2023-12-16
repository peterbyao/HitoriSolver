module Main (main) where

import Hitori
-- import CDCL (solveCDCL)
import ParallelSolver (dpllPar)
import DPLL (parDpllSolve)
import GHC.Arr (Array, bounds)
import System.Environment(getArgs, getProgName)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import PrettyPrint (printArray, printFinalBoard)

{-
Main function for Hitori solving. Boards print nicely now. 

This is how I've been running it: cd into the app dir, then run:
    stack ghci
    :l Main
    main

Let me know if this doesn't work. Sample output is in ./pretty-output.txt
Ignore unused imports/declarations warnings for now... they are for reading files.

TODO:
    * This project isn't just Hitori anymore, so we will need to handle user input of a CNF file,
      which we should solve using the parallel solver
    * Stretch (?) goal: reading in Hitori Boards from file (see: boards directory), and solving those too,
      using the same main function? (Code's all there pending complilation of the rest of the package)
-}


parseFileContent :: String -> Array (Int, Int) Int
parseFileContent content = toArray $ map (map read . words) (lines content)

main :: IO ()
main = do 
    -- NOT WORKING: receiving arguments from a compiled Main. CDCL won't compile, so I can't build with stack yet
    {- 
    args <- getArgs
    pn <- getProgName
    case args of 
        [f] -> do
            handle <- openFile f ReadMode
            contents <- hGetContents handle
            let startBoard = parseFileContent contents
            -- Show starting board
            putStrLn "\nSTARTING BOARD"
            putStrLn $ "Board size: " ++ (\((_,_), (x,y)) -> show x ++ "x" ++ show y) (bounds startBoard)
            putStrLn $ printArray startBoard ++ "\n"

            -- Get rules expressions and combine into single cnf
            let rule1 = formatCNF (toCNF (combineBoolAnd (map combineBoolAnd (getRule1 startBoard)))) startBoard
            let rule2 = formatCNF (toCNF (combineBoolAnd (map combineBoolAnd (getRule2 startBoard)))) startBoard
            let rule3 = getRule3 startBoard
            let cnf = filter (not . null) (rule1 ++ rule2 ++ rule3)

            --let clauses = length cnf
            --let variables = varToInt (Var3 (m-1, n-1, m*n-1)) b
            let (m, n) = getDim startBoard
            -- Find a solution and print it out
            case CDCL.findSat cnf of
                Nothing -> error "UNSAT"
                Just xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

        _ -> error $ "Usage: " ++pn++ " path/to/board.txt" 
        -}
    
    -- hardcoded start board
    let startBoard = [[15, 2, 5, 16, 11, 1, 8, 16, 14, 18, 13, 20, 17, 9, 17, 10, 19, 16, 13, 3],
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
    let b = toArray startBoard
    let (m, n) = getDim b

    -- Show starting board
    putStrLn "STARTING BOARD \n"
    putStrLn $ "Board size: " ++ (\((_,_), (x,y)) -> show x ++ "x" ++ show y) (bounds b)
    putStrLn $ printArray b

    -- Get rules expressions and combine into single cnf
    let rule1 = formatCNF (toCNF (combineBoolAnd (map combineBoolAnd (getRule1 b)))) b
    let rule2 = formatCNF (toCNF (combineBoolAnd (map combineBoolAnd (getRule2 b)))) b
    let rule3 = getRule3 b
    let cnf = filter (not . null) (rule1 ++ rule2 ++ rule3)

    --let clauses = length cnf
    --let variables = varToInt (Var3 (m-1, n-1, m*n-1)) b

    -- Find a solution and print it out
    case parDpllSolve (4 :: Int) cnf of
        Nothing -> error "UNSAT"
        Just xs -> do
            let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
            putStrLn "FINAL SOLUTION \n"
            putStrLn $ printFinalBoard b sol
