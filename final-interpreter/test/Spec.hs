import Interpreter
import StudentTests
import Tests

execStmt input expected = let calculated = exec input env1 mem1
                          in  if expected == calculated
                                then putStrLn $ "Passed: " ++ show input
                                else do putStrLn $ "FAILED: " ++ show input
                                        putStrLn $ "\t\tExpected: " ++ show expected
                                        putStrLn $ "\t\tCalculated: " ++ show calculated

evalStmt input expected = let calculated = eval input env1 mem1
                          in  if expected == calculated
                                then putStrLn $ "Passed: " ++ show input
                                else do putStrLn $ "FAILED: " ++ show input
                                        putStrLn $ "\t\tExpected: " ++ show expected
                                        putStrLn $ "\t\tCalculated: " ++ show calculated

main = do   putStrLn ""
            putStrLn "====== Given Tests ======"
            putStrLn ""
            mapM_ (\(x,y) -> evalStmt x y) evalTests 
            putStrLn ""
            mapM_ (\(x,y) -> execStmt x y) execTests 
            putStrLn ""
            putStrLn "===== Private Tests ====="
            putStrLn ""
            mapM_ (\(x,y) -> evalStmt x y) completeEvalTests
            putStrLn ""
            mapM_ (\(x,y) -> execStmt x y) completeExecTests
            putStrLn ""
            putStrLn "===== Student Tests ====="
            putStrLn ""
            mapM_ (\(x,y) -> evalStmt x y) studentEvalTests
            putStrLn ""
            mapM_ (\(x,y) -> execStmt x y) studentExecTests

