module Tests where

import Data.HashMap.Strict as H
import Lib

execTests :: [(Stmt, Either Val (H.HashMap String Int, H.HashMap Int Val))]
execTests = [   ( WhileStmt (CompOpExp "<=" (VarExp "x") (IntExp 5))
                            (SetStmt "x" (IntOpExp "+" (VarExp "x") (IntExp 1)))
                , Right (fromList [("x",1)],fromList [(1,IntVal 6)])
                )
            ,   ( SeqStmt (SetStmt "f" (IntExp 5))
                          (SetStmt "f" (IntExp 7))
                , Right (fromList [("x",1),("f",2)], fromList [(1,IntVal 0),(2,IntVal 7)])
                )
            ,   ( SetStmt "f" (IntExp 5)
                , Right (fromList [("x",1),("f",2)],(fromList [(1,IntVal 0),(2,IntVal 5)]))
                )
            ,   ( SeqStmt (SetStmt "y" (IntExp 1))
                          (ForStmt "x" (IntExp 1) (IntExp 5)
                                   (SetStmt "y" (IntOpExp "*" (VarExp "x") (VarExp "y")))
                          )
                , Right (fromList [("x",1), ("y",2)],fromList [(1,IntVal 5), (2,IntVal 24)])
                )
            ,   ( TryStmt (SetStmt "x" (IntExp 5))
                          (SetStmt "y" (IntExp 7))
                , Right (fromList [("x",1)],fromList [(1,IntVal 5)])
                )
            ,   ( SetStmt "x" (LetExp [("y", IntExp 7)] (VarExp "y"))
                , Right (fromList [("x",1)],fromList [(1,IntVal 7)])
                )
            ,   ( IfStmt (CompOpExp "==" (VarExp "x") (IntExp 0))
                         (SetStmt "r" (BoolExp True))
                         (SetStmt "r" (BoolExp False))
                , Right (fromList [("x",1), ("r",2)],fromList [(1,IntVal 0), (2, BoolVal True)])
                )
            ,   ( WhileStmt (CompOpExp "<=" (VarExp "x") (IntExp 5))
                                  (SeqStmt (SetStmt "y" (VarExp "x"))
                                           (SetStmt "x" (IntOpExp "+" (VarExp "x") (IntExp 1)))
                                  )
                      , Right (fromList [("x",1),("y",2)],fromList [(1,IntVal 6), (2,IntVal 5)])
                )
            ,   ( ForStmt "x" (IntExp 0) (IntExp 5)
                                (SeqStmt (SetStmt "y" (VarExp "x"))
                                         (SetStmt "x" (IntOpExp "+" (VarExp "x") (IntExp 1)))
                                )
                      , Right (fromList [("x",1),("y",2)],fromList [(1,IntVal 6), (2,IntVal 4)])
                )
            ]

evalTests :: [(Exp, Val)]
evalTests = [ ((IntExp 0), (IntVal 0))
            , ((BoolExp True), (BoolVal True))
            , ((ExnExp "good luck"), (ExnVal "good luck"))
            , ((VarExp "x"), (IntVal 0))
            , ((IfExp (CompOpExp "==" (IntExp 44) (IntExp 0)) (IntExp 44) (IntExp 0)), (IntVal 0))
            , ((LetExp [("x", (IntExp 7))] (IntExp 5)), (IntVal 5))
            , ((IntOpExp "+" (IntExp 40) (IntExp 4)), (IntVal 44))
            , ((CompOpExp "==" (IntExp 44) (IntExp 0)), (BoolVal False))
            ]

completeExecTests :: [(Stmt, Either Val (H.HashMap String Int, H.HashMap Int Val))]
completeExecTests = [ 
                    ]

completeEvalTests :: [(Exp, Val)]
completeEvalTests = [
                    ]

