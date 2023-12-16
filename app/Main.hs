module Main (main) where

import Hitori
import CDCL (solveCDCL) -- sequential
import ParallelSolver (dpllPar, dpllSeq)
import Lookahead (solve)
import CNFtest (readDIMACS)
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
    -- putStrLn "hi"
    args <- getArgs
    pn <- getProgName
    case args of
        [filename, filetype, solverMethod] -> do
            case filetype of
                "cnf" -> solveCNFFile filename solverMethod Nothing
                "txt" -> solveHitori filename solverMethod Nothing
                _ -> error "Invalid filetype: txt or cnf"
        [filename, filetype, solverMethod, depth] -> do
            let d = (read depth) :: Int
            case filetype of
                "cnf" -> solveCNFFile filename solverMethod (Just d)
                "txt" -> solveHitori filename solverMethod (Just d)
                _ -> error "Invalid filetype: txt or cnf"
        _ -> error $ "Usage: "++pn++" filename [txt OR cnf] [dpll OR cdcl] [optional: parallelism depth]"

solveCNFFile :: String -> String -> Maybe Int -> IO()
solveCNFFile cnfFileName alg depth = do
    putStrLn $ "input file: " ++ cnfFileName
    cnf <- readDIMACS cnfFileName
    case depth of
        Nothing -> case alg of
            "dpll" -> print $ dpllSeq cnf []
            "cdcl" -> print $ solveCDCL cnf
            _ -> error "Invalid algorithm"
        Just d -> case alg of
            "dpll" -> print $ dpllPar d cnf []
            "cdcl" -> print $ solve cnf
            _ -> error "Invalid algorithm"
solveHitori :: String -> String -> Maybe Int -> IO()
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

    case depth of
        Nothing -> case alg of
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
        Just d -> case alg of
            "dpll" -> case dpllPar d cnf [] of
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
            
