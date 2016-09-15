{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

import Data.List as L
import Data.HashMap.Strict as H

data Stmt = SeqStmt Stmt Stmt
          | SetStmt String Exp
          | IfStmt Exp Stmt Stmt
          | WhileStmt Exp Stmt
          | ForStmt String Exp Exp Stmt
          | TryStmt Stmt Stmt
  deriving Eq

instance Show Stmt where
    show (SeqStmt s1 s2)        = show s1 ++ "; " ++ show s2
    show (SetStmt x e)          = x ++ " := " ++ show e
    show (IfStmt e1 s1 s2)      = "if (" ++ show e1 ++ ")"
                                    ++ " then { " ++ show s1 ++ " }"
                                    ++ " else { " ++ show s2 ++ " }"
    show (WhileStmt e1 s1)      = "while (" ++ show e1 ++ ")"
                                    ++ " do { " ++ show s1 ++ " } od"
    show (ForStmt x e1 e2 s1)   = "for " ++ x ++ " from " ++ show e1 ++ " to " ++ show e2
                                    ++ " do { " ++ show s1 ++ "} od"
    show (TryStmt s1 s2)        = "try { " ++ show s1 ++ " }"
                                    ++ "on exception { " ++ show s2 ++ " }"

data Exp = IntExp Int
         | BoolExp Bool
         | ExnExp String
         | VarExp String
         | IfExp Exp Exp Exp
         | LetExp [(String, Exp)] Exp
         | IntOpExp String Exp Exp
         | CompOpExp String Exp Exp
  deriving Eq

instance Show Exp where
    show (IntExp n)             = show n
    show (BoolExp b)            = show b
    show (ExnExp exn)           = "exception: " ++ exn
    show (VarExp s)             = s
    show (IfExp e1 e2 e3)       = "if " ++ show e1
                                    ++ " then " ++ show e2
                                    ++ " else " ++ show e3
    show (LetExp xs exp)        = let ls = Prelude.map (\(x, y) -> x ++ " := " ++ show y) xs
                                  in  "let [" ++ intercalate ", " ls ++ "] in " ++ show exp
    show (IntOpExp op e1 e2)    = show e1 ++ " " ++ op ++ " " ++ show e2
    show (CompOpExp op e1 e2)   = show e1 ++ " " ++ op ++ " " ++ show e2

data Val = IntVal Int
         | BoolVal Bool
         | ExnVal String
  deriving Eq

instance Show Val where
  show (IntVal n)   = show n
  show (BoolVal b)  = show b
  show (ExnVal exn) = "exception: " ++ exn

{- Our Type Synonyms -}
type Env    = H.HashMap String Int
type Store  = H.HashMap Int Val
type Result = (Env, Store)

{- Our BinOps -}
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("%", mod)
                    ]

compOps = H.fromList [ ("==", (==))
                     , ("!=", (/=))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("<", (<))
                     , (">", (>))
                     ]
