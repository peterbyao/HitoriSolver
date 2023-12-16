module CNFtest where

import System.IO(hGetLine, hClose, withFile, hIsEOF, IOMode(ReadMode), stderr, hPutStrLn)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure)
import CDCL (solveCDCL)
import DPLL (seqDPLLSolve, parDpllSolve)
import Lookahead (solve, getInitCube)
import ParallelSolver (dpllSeq, dpllPar)

type Literal = Int
type Clause  = [Literal]
type Formula = [Clause]

readDIMACS :: String -> IO [[Int]]
readDIMACS path =
    withFile path ReadMode $ \h -> parseFile h
        where
            parseFile hdl = do
                isEOF <- hIsEOF hdl
                if isEOF then do
                    hClose hdl
                    return []
                else do
                    line <- hGetLine hdl
                    case head $ words $ line of
                        "c" -> parseFile hdl
                        "p" -> parseFile hdl
                        _  -> do
                                d <- getClause line
                                rest <- parseFile hdl
                                return (d:rest)

getClause :: String -> IO [Int]
getClause clause = do
  let c = (map read $ init $ words clause) :: [Int]
  return c

main :: IO ()
main = do
    cnf <- readDIMACS "CBS_k3_n100_m411_b90_999.cnf"

    putStrLn $ show $ length $ getInitCube cnf


    --case CDCL.solveCDCL cnf of
    case Lookahead.solve cnf of
    --case DPLL.solve cnf of
        Nothing -> error "UNSAT"
        Just xs -> do
            putStrLn $ show xs

    


{-
    case ParallelSolver.dpllSeq cnf [] of
        xs -> do
            putStrLn $ show xs
            -}

{-}
    case ParallelSolver.dpllPar 40 cnf [] of
        xs -> do
            putStrLn $ show xs
-}

{- NOT WORKING 
    args <- getArgs
    case args of
        [filename] -> do
                        cnf <- readDIMACS filename
                        case ParallelSolver.dpllPar 40 cnf [] of
                            xs -> do
                                putStrLn $ show xs
        _          -> do
                        pn <- getProgName --Usage message
                        hPutStrLn stderr $ "Usage: "++pn++" <filename>"
                        exitFailure --Terminate the program
            
-}