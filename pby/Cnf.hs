module Cnf where

import Hitori

{-
Expr -> DIMACS .cnf converter.
 The CNF file format is an ASCII file format, which is as follows:

    * The file may begin with comment lines. The first character of each comment line must be a lower case letter "c". 
      Comment lines typically occur in one section at the beginning of the file, but are allowed to appear throughout 
      the file.
    * The comment lines are followed by the "problem" line. This begins with a lower case "p" followed by a space, 
      followed by the problem type, which for CNF files is "cnf", followed by the number of variables followed by the
      number of clauses.
    * The remainder of the file contains lines defining the clauses, one by one.
    * A clause is defined by listing the index of each positive literal, and the negative index of each negative literal. 
      Indices are 1-based, and for obvious reasons the index 0 is not allowed.
    * The definition of a clause may extend beyond a single line of text.
    * The definition of a clause is terminated by a final value of "0".
    * The file terminates after the last clause is defined.

-}

-- Function to convert an expression to CNF clauses. Remember that in DIMACS literals are 1-indexed.
exprToCNF :: Expr -> [[(Int, Bool)]]
exprToCNF (Var (i, j)) = [[(varIndex (i+1) (j+1), True)]]
exprToCNF (And e1 e2) = exprToCNF e1 ++ exprToCNF e2
exprToCNF (Or e1 e2) = [concat (exprToCNF e1 ++ exprToCNF e2)]
exprToCNF (Not e) = [map (\(i, b) -> (i, not b)) (concat $ exprToCNF e)]
exprToCNF (Const True) = [[]]
exprToCNF (Const False) = []

-- Function to calculate the 1-D index for the (i, j) pair in the Var
varIndex :: Int -> Int -> Int
varIndex i j = i * 1000 + j

-- given an Expr in CNF, write to a CNF file in standard DIMACS notation.
createCNFFile :: Expr -> Int -> Int -> String -> IO()
createCNFFile expr numClauses numLiterals filename = withFile filename WriteMode $ \handle -> do
    hPutStrLn handle $ "c CNF file generated from Haskell Expr representing a Hitori Board"
    hPutStrLn handle $ "c the expression is: " ++ show expr
    hPutStrLn handle $ "p cnf " ++ show numLiterals ++ " " ++ show numClauses
    mapM_ (writeClause handle) (exprToCNF expr)

-- Write a single clause to the file.
writeClause :: Handle -> [(Int, Bool)] -> IO ()
writeClause handle clause = do
    hPutStrLn handle $ unwords (map (\(i, b) -> if b then show i else show (-i)) clause) ++ " 0"

main :: IO ()
main = do
   -- test for .cnf file creation
    let expr =  And (Or (Var (0, 0)) (Not (Var (1, 1)))) (Or (Var (1, 0)) (Or (Var (1, 1)) (Not (Var (0, 0)))))
    createCNFFile expr 3 2 "output.cnf"

