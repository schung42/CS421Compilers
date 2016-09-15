{-----------------------------------
 - Interpreter.hs
 - v1.0
 -----------------------------------}

module Interpreter where

-- Language Representation
import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec
import Parser

fixMe = error "fix me!"

liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBool _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal (op x y)
liftCompOp _ _ _ = ExnVal "Cannot lift"

{--| This is part of problem 2 exec--}
first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x
concat' a (b,c,d) = (a ++ b, c,d)
boolEq (BoolVal a) b = a==b

--LetExp Helper Function
specInsert [] env = env
specInsert (x:xs) env = specInsert xs (H.insert (fst x) (eval (snd x) env) env)

getenvbabez parameters values env = specInsert (zip parameters values) env

{-----------------------------------
 - eval: The Evaluator
 -----------------------------------}
eval :: Exp -> Env -> Val
eval (IntExp e) _ = (IntVal e)
eval (BoolExp e) _ = (BoolVal e)
eval (VarExp e) env = 
    case H.lookup e env of
      Just v->v
      Nothing-> error $ "Undefined: " ++ e

eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op intOps
    in liftIntOp f v1 v2

eval (BoolOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op boolOps
    in liftBoolOp f v1 v2

eval(CompOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
        Just f = H.lookup op compOps
    in liftCompOp f v1 v2

eval (LetExp newEnv e1) env = eval e1 (specInsert newEnv env)

eval(FunExp v e1) env = 
    CloVal v e1 env 

eval (AppExp e1 vals) env = 
    case (eval e1 env) of
      (CloVal param body cenv) -> eval body (specInsert (zip param vals) cenv)
      _                        -> ExnVal "Expression not a Closure"

eval (IfExp c t e) env = 
  case (eval c env) of
    BoolVal True -> eval t env
    _            -> eval e env

eval _ _ = fixMe


{-----------------------------------
 - exec
 -----------------------------------}
exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
   where val = show $ eval e env

{--|problem 2--}
exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (e:es)) penv env = concat' (first command) (exec (SeqStmt es) (second command) (third command))
  where command = exec e penv env

{--|problem 4 conditionals--}
exec (IfStmt c t e) penv env = 
  if boolEq (eval c env) True
    then exec t penv env
  else exec e penv env

exec (SetStmt initString whatYouWantItToBe) penv env = 
  ("", penv, H.insert initString (eval whatYouWantItToBe env) env)

exec (ProcedureStmt f params body) penv env =
  ("", (H.insert f (ProcedureStmt f params body) penv), env)

exec (CallStmt f value) penv env =
  case (H.lookup f penv) of 
    Just (ProcedureStmt f params stmt) -> ( (first (exec stmt penv (getenvbabez params value env))), penv, (getenvbabez params value env))




exec _ _ _ = fixMe



{-----------------------------------
 - repl
 -----------------------------------}
repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main = do
  putStrLn "Welcome to your interpreter!"
  repl H.empty H.empty [] "stdin"
