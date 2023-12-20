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
import System.FilePath
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


-- Solves a CNF file using a selected algorithm. If depth is not needed, it is ignored
solveCNFFile :: String -> String -> Int -> IO()
solveCNFFile cnfFileName alg depth = do
    putStrLn $ "input file: " ++ cnfFileName
    cnf <- readDIMACS cnfFileName

    -- Selecting the algorithm
    case alg of
            "dpllPar" -> print $ dpllPar depth cnf []
            "dpllSeq" -> print $ dpllSeq cnf []
            "cubeAndConquer" -> print $ solveCube cnf
            "cdclSeq" -> print $ solveCDCL cnf
            "lookaheadPar" -> do
                print "WARNING: Look-Ahead is expensive"
                print $ lookAheadPar depth cnf []
            "lookaheadSeq" -> do 
                print "WARNING: Look-Ahead is expensive"
                print $ lookAheadSeq cnf []
            _ -> error "Invalid algorithm"

-- Solves a Hitori problem using a selected algorithm. If depth is not needed, it is ignored
solveHitori :: String -> String -> Int -> IO()
solveHitori boardFilename alg depth = do
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

    -- Selecting the solving algorithm
    case alg of
            "dpllSeq" -> case dpllSeq cnf [] of
                [] -> error "UNSAT"
                xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

            "dpllPar" -> case dpllPar 20 cnf [] of
                [] -> error "UNSAT"
                xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

            "cdclSeq" -> case solveCDCL cnf of
                Nothing -> error "UNSAT"
                Just xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

            "cubeAndConquer" -> case solveCube cnf of
                Nothing -> error "UNSAT"
                Just xs -> do
                    let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                    putStrLn "FINAL SOLUTION"
                    putStrLn $ printFinalBoard startBoard sol

            "lookaheadSeq" -> do
                print "WARNING: Look-Ahead is expensive"
                case lookAheadSeq cnf [] of
                    [] -> error "UNSAT"
                    xs -> do
                        let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                        putStrLn "FINAL SOLUTION"
                        putStrLn $ printFinalBoard startBoard sol

            "lookaheadPar" -> do
                print "WARNING: Look-Ahead is expensive"
                case lookAheadPar depth cnf [] of
                    [] -> error "UNSAT"
                    xs -> do
                        let sol = filter (\x -> abs x <= (m*n)) (getShadedBool xs)
                        putStrLn "FINAL SOLUTION"
                        putStrLn $ printFinalBoard startBoard sol
            
            _ -> error "Invalid algorithm"
            
-- Main function takes in arguments to be passed to 
main :: IO ()
main = do 
    args <- getArgs
    pn <- getProgName
    case args of
        [filename, solverMethod, depth] -> do
            let d = read depth
            let ext = takeExtension filename
            case ext of
                ".cnf" -> solveCNFFile filename solverMethod d
                ".txt" -> solveHitori filename solverMethod d
                _ -> error "Invalid filetype: txt or cnf"
        [filename, solverMethod] -> do
            let ext = takeExtension filename
            case ext of
                ".cnf" -> 
                    case elem solverMethod ["cdclSeq", "lookaheadSeq", "dpllSeq"] of
                        True -> solveCNFFile filename solverMethod 0
                        False -> error "Invalid arguments: parallel solvers require depth parameter, or invalid algorithm"
                ".txt" -> 
                    case elem solverMethod ["cdclSeq", "lookaheadSeq", "dpllSeq"] of
                        True -> solveHitori filename solverMethod 0
                        False -> error "Invalid arguments: parallel solvers require depth parameter, or invalid algorithm"
                _ -> error "Invalid filetype: txt or cnf"
        _ -> error $ "Usage: "++pn++" filename [txt OR cnf] [dpll OR cdcl OR lookahead] [par OR seq]"