import TestModules.CNFReader(readDIMACS)
import TestModules.Lookahead (lookAheadPar)

main :: IO()
main = do
    cnf <- readDIMACS "../CBS_k3_n100_m411_b90_999.cnf"
    case lookAheadPar 1 cnf [] of
        [] -> error "UNSAT"
        sol -> putStrLn $ "the solution is: " ++ show sol