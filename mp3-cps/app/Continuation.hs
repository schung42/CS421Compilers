module Continuation where
import Lib

fixMe = error "fix me!"

repl () =
  do putStr "CPS> "
     input <- getLine
     case parseDecl input of
        Right x -> let result = cpsDecl x
                    in do putStrLn "Pretty Result: "
                          putStrLn $ toStr result
                          putStrLn "Details: "
                          putStrLn $ show result
                          putStrLn ""
                          repl ()
        Left x -> do putStrLn $ show x
                     repl ()

{--------------------------------------
 - Problem 1: factk 
 --------------------------------------}
factk :: Integer -> (Integer -> a) -> a
factk 0 k = k 1
factk x k = factk (x-1)(\v-> k (v * x))


{--------------------------------------
 - Problem 2: evenoddk
 --------------------------------------}
evenoddk :: Integral r => [r] -> (r -> t) -> (r -> t) -> t
evenoddk [x] kodd keven 
    |  mod x 2 == 1 = kodd x
    | otherwise = keven x
evenoddk (x:xs) kodd keven 
    | mod x 2 == 1 = evenoddk (xs) (\v-> kodd(v + x)) keven
    | otherwise = evenoddk (xs) kodd (\v->keven(v+x))

{--------------------------------------
 - Problem 3: isSimple
 --------------------------------------}
isSimple :: Exp -> Bool
isSimple (IntExp e1) = True
isSimple (VarExp op) = True
isSimple (LamExp op e2) = True

isSimple (OpExp op e1 e2) = isSimple(e1) && isSimple(e2)
isSimple (AppExp e1 e2) = False 
isSimple (IfExp e1 e2 e3) = isSimple(e1) && isSimple(e2) && isSimple(e3)

{--------------------------------------
 - Problem 4: cpsDecl, cpsExp
 --------------------------------------}
cpsDecl :: Stmt -> Stmt
cpsDecl (Decl name params body) = Decl (name ++ "k") (params ++ ["k"]) (fst (cpsExp body (VarExp "k") 1))

cpsExp :: Exp -> Exp -> Integer -> (Exp,Integer)
{--|CPS Transform of IfExp--}
cpsExp body@(IfExp c t e) k num = 
    {--|if the 'conditional' is simple, we do a cps
      transform on 'then' and 'else'. Since cpsExp
      returns a tuple, we need to pair them with a number
      this number is the count of the number of times we've
      'lambda'-ed
      --}
    if isSimple c 
      then let (t1,n1) = (cpsExp t k num) 
               (e1,n2) = (cpsExp e k n1)
           in (IfExp c t1 e1 ,n2)
    else
      let (v,n1) = (gensym num)
          (t1, n2) = (cpsExp t k n1)
          (e1, n3) = (cpsExp e k n2)
      in cpsExp c (LamExp v (IfExp (VarExp v) t1 e1)) n3

{--|CPS Transform of OpExp--}
{--|This is pretty much the same idea, except we're
including the idea of an OpExp beinc applied to our Exps--}
cpsExp body@(OpExp op e1 e2) k num = 
    if (isSimple e1) && (isSimple e2)
      then (AppExp k body,num)

    else if isSimple e2
      then let (v,n1) = (gensym num)
      in  cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) n1

    else if isSimple e1
      then let (v,n1) = (gensym num)
      in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) n1

    else
      let (v1, n1) = (gensym num)
          (v2, n2) = (gensym n1)
          (e2', n3) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) n2
      in cpsExp e1 (LamExp v1 e2') n3
      
{--|CPS Transform of AppExp--}
cpsExp body@(AppExp func e) k num = 
  if isSimple e
    then ((AppExp (AppExp func e) k),num)
  else
    let (v, n1) = (gensym num)
    in cpsExp e (AppExp (AppExp func (VarExp v)) k) n1 -- There should be a LamExp in here somewhere

cpsExp body k num = ((AppExp k body), num)



{--------------------------------------
 - Helper Functions
 --------------------------------------}

gensym :: Integer -> (String,Integer)
gensym i = ("v" ++ show i, i + 1)
