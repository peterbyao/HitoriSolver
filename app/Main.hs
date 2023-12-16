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

-- Parses a board (text file) into an Array for solving.
parseFileContent :: String -> Array (Int, Int) Int
parseFileContent content = toArray $ map (map read . words) (lines content)

main :: IO ()
main = do  
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

            let (m, n) = getDim startBoard
            -- Find a solution and print it out
            case parDpllSolve (4 :: Int) cnf of
                Nothing -> error "UNSAT"
                Just xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

        _ -> error $ "Usage: " ++pn++ " path/to/board.txt" 

    
