module CNFtest (readDIMACS) where

import System.IO(hGetLine, hClose, withFile, hIsEOF, IOMode(ReadMode), stderr, hPutStrLn)

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