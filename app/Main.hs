{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Main (main) where

import Hitori
    ( combineBoolAnd,
      formatCNF,
      getDim,
      getRule1,
      getRule2,
      getRule3,
      getShadedBool,
      toArray,
      toCNF )

import CNFReader (readDIMACS)
import CDCL (solveCDCL) -- sequential
import DPLL (dpllPar, dpllSeq)
import CubeAndConquer (solveCube)
import Lookahead (lookAheadSeq, lookAheadPar)
import GHC.Arr (Array, bounds)
import System.Environment(getArgs, getProgName)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import PrettyPrint (printArray, printFinalBoard)

{-
Main function for Hitori solving. Boards print nicely now. 
    
Build/run instructions:
    stack build
    stack run <path-to-file> <txt | cnf> <dpll | cdcl> [parallelism-depth]
    For example, solving a
-}

-- Parses a board (text file) into an Array for solving.
parseFileContent :: String -> Array (Int, Int) Int
parseFileContent content = toArray $ map (map read . words) (lines content)

main :: IO ()
main = do 
    args <- getArgs
    pn <- getProgName
    case args of
        [filename, filetype, solverMethod, "seq"] -> do
            case filetype of
                "cnf" -> solveCNFFile filename solverMethod False
                "txt" -> solveHitori filename solverMethod False
                _ -> error "Invalid filetype: txt or cnf"
        [filename, filetype, solverMethod, "par"] -> do
            case filetype of
                "cnf" -> solveCNFFile filename solverMethod True
                "txt" -> solveHitori filename solverMethod True
                _ -> error "Invalid filetype: txt or cnf"
        _ -> error $ "Usage: "++pn++" filename [txt OR cnf] [dpll OR cdcl OR lookahead] [par OR seq]"

solveCNFFile :: String -> String -> Bool -> IO()
solveCNFFile cnfFileName alg para = do
    putStrLn $ "input file: " ++ cnfFileName
    cnf <- readDIMACS cnfFileName
    case para of
        True -> case alg of
            "dpll" -> print $ dpllPar 20 cnf []
            -- cdcl in parallel is cube and conquer, with default depth 2
            "cdcl" -> print $ solveCube cnf
            "lookahead" -> print $ lookAheadPar 1 cnf []
            _ -> error "Invalid algorithm"
        False -> case alg of
            "dpll" -> print $ dpllSeq cnf []
            "cdcl" -> print $ solveCDCL cnf
            "lookahead" -> print $ lookAheadSeq cnf []
            _ -> error "Invalid algorithm"

solveHitori :: String -> String -> Bool -> IO()
solveHitori boardFilename alg para = do
    handle <- openFile boardFilename ReadMode        
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

    case para of
        False -> case alg of
            "dpll" -> case dpllSeq cnf [] of
                [] -> error "UNSAT"
                xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

            "cdcl" -> case solveCDCL cnf of
                Nothing -> error "UNSAT"
                Just xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol
            _ -> error "Invalid algorithm"
        True -> case alg of
            "dpll" -> case dpllPar 20 cnf [] of
                [] -> error "UNSAT"
                xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol
            "cdcl" -> case solveCube cnf of
                Nothing -> error "UNSAT"
                Just xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol
            _ -> error "Invalid algorithm"
            
