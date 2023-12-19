module CNFReader (readDIMACS, checkSatisfiability) where

{-

 Names: Peter Yao and Ava Hajratwala
 Uni: pby2101 and ash2261

 ------------------------------

 COMS 4995 001 Parallel Functional Programming

 Final Project: Hitori Solver

 This file was created to test our CNF solvers on a variety of problems. It includes a DIMACS
 file parser to convert CNF formulas to [[Int]] that can be interpreted by our SAT solvers.
-}

import System.IO(hGetLine, hClose, withFile, hIsEOF, IOMode(ReadMode))
import Data.List

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

checkSatisfiability :: Formula -> [Int] -> Bool
checkSatisfiability [] _ = True
checkSatisfiability _ [] = True
checkSatisfiability (x:xs) sols = if null (intersect x sols) then False
                                    else checkSatisfiability xs sols

-- main :: IO ()
-- main = do

--     cnf <- readDIMACS "CBS_k3_n100_m411_b90_999.cnf"
--     --cnf <- readDIMACS "zebra_v155_c1135.cnf"
    
--     {-
--     Our CDCL and Cube and Conquer implementations return Maybe solutions
--     -}


--     --case solveCDCL cnf of
--     case solveCube cnf of
--         Nothing -> error "UNSAT"
--         Just sol -> do
--             --putStrLn $ show sol --I
--             putStrLn $ show (checkSatisfiability cnf sol)



-- {-
--     --case lookAheadSeq cnf [] of
--     --case lookAheadPar cnf 10 [] of
--     --case dpllSeq cnf [] of
--     case dpllPar 10 cnf [] of
--         xs -> do
--             putStrLn $ show xs
-- -}