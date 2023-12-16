module PrettyPrint (printArray, printFinalBoard) where

import GHC.Arr (Array, bounds, (!))
import Data.Set (fromList, member)

{- 
AH (12/15): added a family of prettyprint functions to print out the Hitori Board start and end states nicely.
See them in action in app/Main.hs.
-}

printBorder :: Array (Int, Int) Int -> String
printBorder arr = "+" ++ concat (replicate (m+1) (replicate cellWidth '-' ++ "+"))
  where
    m = x1 - x0
    ((x0, _), (x1, _)) = bounds arr
    cellWidth = 4 -- can change if numbers get bigger

printArrayRow :: Array (Int, Int) Int -> String -> String
printArrayRow arr rowString =  printBorder arr ++ "\n" ++ rowString ++ " |" ++ "\n"

printArray :: Array (Int, Int) Int -> String
printArray arr = concatMap (printArrayRow arr) [unwords [show' (arr ! (x, y)) | x <- [0..m]] | y <- [0..n]] ++ printBorder arr
                    where
                        ((x0, y0), (x1, y1)) = bounds arr
                        m = x1-x0
                        n = y1-y0
                        show' x 
                            | x < 10 = "| " ++ show x ++ " " 
                            | otherwise =  "| " ++ show x
printFinalBoard :: Array (Int, Int) Int -> [Int] -> String
printFinalBoard arr sol = concatMap (printArrayRow arr) [unwords [if ((m+1) * x + (y+1)) `member` s then "| ■ " else show' (arr ! (x, y))
                        | x <- [0..m]] | y <- [0..n]] ++ printBorder arr
                            where
                                ((x0, y0), (x1, y1)) = bounds arr
                                m = x1-x0
                                n = y1-y0
                                show' x 
                                    | x < 10 = "| " ++ show x ++ " " 
                                    | otherwise =  "| " ++ show x
                                s = fromList sol