import TestModules.CNFReader (readDIMACS)
import TestModules.CubeAndConquer (solveCube)

main :: IO()
main = do
    cnf <- readDIMACS "../CBS_k3_n100_m411_b90_999.cnf"
    case solveCube cnf of
        Nothing -> error "UNSAT"
        Just sol -> putStrLn $ "the solution is: " ++ show sol