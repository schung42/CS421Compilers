{-----------------------------------
 - Tests.hs
 - v1.0.1
 -----------------------------------}

module Tests where

import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.HashMap.Strict as H
import Parser
import Interpreter

tests = [
          -- quit
          ("quit;", ""),

          -- intexp
          ("print 1;", "1"),

          -- if/then/else stmts
          ("if true then print 1; else print 0; fi;", "1"),

          -- assignment
          ("if true then do x := 1; print x; od; else do x := 0; print x; od; fi;", "1"),

          -- integer operations
          ("print 1+2;", "3"),

          -- boolean operations
          ("if true and true then print 1; else print 0; fi;", "1"),

          -- comparison operations
          ("if (4 > 3) then print 1; else print 0; fi;", "1"),

          -- let expressions
          ("do x := let[x:=5] (if x > 3 then 1 else 0 fi) end; print x; od;", "1"),

          -- functions/function applications
          ("do f := fn[x,y] (x + y) end; print (apply f(1,2)); od;", "3"),

          -- procedures
          ("do procedure foo() print 1; endproc call foo(); od;", "1"),

          -- if/then/else exps
          ("do x := (if true then 1 else 0 fi); print x; od;", "1")
        ]

{-----------------------------------
 - Helper Functions for Testing
 -----------------------------------}
showStmt :: String -> String
showStmt s = case parse stmt "" s of
               Right x -> show x
               Left x -> show x

evalFull :: PEnv -> Env -> String -> String -> Result
evalFull penv env buf expr = case parse stmt "" expr of
                  Right QuitStmt -> (buf, penv, env)
                  Right x -> evalFull nupenv nuenv (buf ++ nuresult) "quit;"
                    where (nuresult,nupenv,nuenv) = exec x penv env
                  Left x -> ((show x), penv, env)

evalTester = evalFull H.empty H.empty ""


showAll = (mapM_ (\(expr, ans) -> putStrLn (showStmt expr)) tests)

evalAll = (map (\(expr, ans) -> (evalTester expr)) tests)
evaluateAll = (mapM_ (\s -> putStrLn (">>> " ++ show s)) evalAll)

testAll = (map (\(expr, ans) -> ((\(out, _, _) -> out == ans) (evalTester expr))) tests)
