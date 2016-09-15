{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Data.List
import Text.ParserCombinators.Parsec

data Stmt = Decl String [String] Exp
    deriving (Show,Eq)

class ToStr a where
  toStr :: a -> String

instance ToStr Stmt where
  toStr (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (toStr exp)

data Exp = IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LamExp String Exp
    deriving (Show,Eq)

instance ToStr Exp where
  toStr (VarExp s) = s
  toStr (IntExp i) = show i
  toStr (AppExp f e) = toStr f ++ " " ++ toStr e
  toStr (OpExp op e1 e2) = "(" ++ toStr e1 ++ " " ++ op ++ " " ++ toStr e2 ++ ")"
  toStr (IfExp e1 e2 e3) = "(if " ++ toStr e1 ++ " then " ++ toStr e2 ++ " else " ++ toStr e3 ++ ")"
  toStr (LamExp x e) = "(\\" ++ x ++ " -> " ++ (toStr e) ++ ")"

-- The Parser

symbol s = do string s
              spaces
              return s

int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Integer)

keywords = ["if", "then", "else"]

var = try $ do v <- many1 letter <?> "an identifier"
               spaces
               if (any (== v) keywords)
                  then fail "keyword"
                  else return v

oper = do op <- many1 (oneOf "+-*/<>=") <?> "an operator"
          spaces
          return op

parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp = do i <- int
            return $ IntExp i

varExp = do v <- var
            return $ VarExp v

mulOp =    do try $ do { symbol "*" ; return $ OpExp "*" }
       <|> do try $ do { symbol "/" ; return $ OpExp "/" }

addOp =    do try $ do { symbol "+" ; return $ OpExp "+" }
       <|> do try $ do { symbol "-" ; return $ OpExp "-" }

compOp =   do try $ do { symbol "<" ; return $ OpExp "<" }
       <|> do try $ do { symbol ">" ; return $ OpExp ">" }
       <|> do try $ do { symbol "<=" ; return $ OpExp "<=" }
       <|> do try $ do { symbol ">=" ; return $ OpExp ">=" }
       <|> do try $ do { symbol "/=" ; return $ OpExp "/=" }
       <|> do try $ do { symbol "==" ; return $ OpExp "==" }

ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           return $ IfExp e1 e2 e3

lamExp = do try $ symbol "\\"
            param <- var
            symbol "->"
            body <- expr
            return $ LamExp param body

appExp = do e1 <- expr
            e2 <- expr
            return $ AppExp e1 e2

expr = arith `chainl1` compOp
arith = term `chainl1` addOp
term = factor `chainl1` mulOp
factor = app
app = do f <- many1 atom
         return $ foldl1 AppExp f

atom = intExp
   <|> ifExp
   <|> lamExp
   <|> varExp
   <|> parens expr

 -- Declarations

decl = do f <- var
          params <- many1 var
          symbol "="
          body <- expr
          return $ Decl f params body

parseExp str = parse expr "stdin" str
parseDecl str = parse decl "stdin" str
