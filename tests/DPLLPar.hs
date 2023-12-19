import TestModules.DPLL (dpllPar)
import TestModules.CNFReader (readDIMACS)

main :: IO()
main = do
    cnf <- readDIMACS "../CBS_k3_n100_m411_b90_999.cnf"
    case dpllPar 20 cnf [] of
        [] -> error "UNSAT"
        sol -> putStrLn $ "the solution is: " ++ show sol