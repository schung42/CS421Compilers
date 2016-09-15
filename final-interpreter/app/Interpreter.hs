module Interpreter where

import qualified Data.HashMap.Strict as H
import Lib
import Debug.Trace

fixMe = error "fix me"

{- (2) -}
insertEnv :: String -> Val -> Env -> Store -> Result
insertEnv x v env mem = fixMe
    where freshAddr = (H.size mem) + 1

{--------------------------------
 - eval
 --------------------------------}

eval :: Exp -> Env -> Store -> Val

{- (1) -}
eval (IntExp n) env mem     = IntVal n
eval (BoolExp b) env mem    = BoolVal b
eval (ExnExp exn) env mem   = ExnVal exn

{- (3) -}
eval (VarExp b) env mem = fixMe

{- (4) -}
eval (IfExp e1 e2 e3) env mem = fixMe

{- (5) -}
eval (LetExp xs exp) env mem = fixMe

{- (6) -}
eval (IntOpExp op e1 e2) env mem = fixMe

eval (CompOpExp op e1 e2) env mem = fixMe

{--------------------------------
 - exec
 --------------------------------}

exec :: Stmt -> Env -> Store -> Either Val Result

{- (7) -}
exec (SeqStmt s1 s2) env mem = fixMe

{- (8) -}
exec (SetStmt x e) env mem = fixMe

{- (9) -}
exec (IfStmt e s1 s2) env mem = fixMe

{- (10) -}
exec w@(WhileStmt e s) env mem = fixMe

{- (11) -}
exec (ForStmt x e1 e2 s) env mem = fixMe

{- (12) -}
exec (TryStmt s1 s2) env mem = fixMe

{--------------------------------
 - Test Environments/Stores
 --------------------------------}

env1 :: Env
env1 = H.fromList ([ ("x", 1) ])

mem1 :: Store
mem1 = H.fromList ([ (1, IntVal 0) ])
