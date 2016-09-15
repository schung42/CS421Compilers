{-----------------------------------
 - Parser.hs
 - v1.3
 -----------------------------------}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where
import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec

-- Our datatypes

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
   deriving Show

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
   deriving Show

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String

instance Show Val where
   show (IntVal i) = show i
   show (BoolVal i) = show i
   show (CloVal xs body env) = "<" ++ show xs ++ ", "
                                   ++ show body ++ ", "
                                   ++ show env ++ ">"
   show (ExnVal s) = "\nexn: " ++ s

-- Type for the symbol dictionary

type Env = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

-- Primitives

intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", div) ]
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||)) ]
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("!=", (/=))
                     , ("==", (==)) ]

-- Parser, given for you this time.

-- Lexicals

run p s =
   case parse p "<stdin>" s of
      Right x -> x
      Left x -> error $ show x

symbol s = do string s
              spaces
              return s

int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Int)

var = do v <- many1 letter <?> "an identifier"
         spaces
         return v

parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

-- Expressions

intExp = do i <- int
            return $ IntExp i

boolExp = do { symbol "true" ; return $ BoolExp True }
      <|> do { symbol "false"; return $ BoolExp False}

varExp = do v <- var
            return $ VarExp v

mulOp =    do try $ do { symbol "*" ; return $ IntOpExp "*" }
       <|> do try $ do { symbol "/" ; return $ IntOpExp "/" }

addOp =    do { symbol "+" ; return $ IntOpExp "+" }
       <|> do { symbol "-" ; return $ IntOpExp "-" }

andOp = do try $ symbol "and"
           return $ BoolOpExp "and"

orOp = do try $ symbol "or"
          return $ BoolOpExp "or"

compOp =   do try $ do { symbol "<=" ; return $ CompOpExp "<=" }
       <|> do try $ do { symbol ">=" ; return $ CompOpExp ">=" }
       <|> do try $ do { symbol "!=" ; return $ CompOpExp "!=" }
       <|> do try $ do { symbol "==" ; return $ CompOpExp "==" }
       <|> do try $ do { symbol "<" ; return $ CompOpExp "<" }
       <|> do try $ do { symbol ">" ; return $ CompOpExp ">" }

ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           symbol "fi"
           return $ IfExp e1 e2 e3

funExp = do try $ symbol "fn"
            symbol "["
            params <- var `sepBy` (symbol ",")
            symbol "]"
            body <- expr
            symbol "end"
            return $ FunExp params body

letExp = do try $ symbol "let"
            symbol "["
            params <- (do v <- var
                          symbol ":="
                          e <- expr
                          return (v,e)
                      )
                      `sepBy` (symbol ";")
            symbol "]"
            body <- expr
            symbol "end"
            return $ LetExp params body

appExp = do try $ symbol "apply"
            efn <- expr
            symbol "("
            exps <- expr `sepBy` (symbol ",")
            symbol ")"
            return $ AppExp efn exps

expr = disj `chainl1` try(orOp)
disj = conj `chainl1` try(andOp)
conj = arith `chainl1` try(compOp)
arith = term `chainl1` try(addOp)
term = factor `chainl1` try(mulOp)
factor = atom

atom = intExp
   <|> funExp
   <|> ifExp
   <|> letExp
   <|> try boolExp
   <|> appExp
   <|> varExp
   <|> parens expr

-- Statements

quitStmt = do try $ symbol "quit"
              symbol ";"
              return QuitStmt

printStmt = do try $ symbol "print"
               e <- expr
               symbol ";"
               return $ PrintStmt e

setStmt = do v <- var
             symbol ":="
             e <- expr
             symbol ";"
             return $ SetStmt v e

ifStmt = do try $ symbol "if"
            e1 <- expr
            symbol "then"
            s2 <- stmt
            symbol "else"
            s3 <- stmt
            symbol "fi"
            return $ IfStmt e1 s2 s3

procStmt = do try $ symbol "procedure"
              name <- var
              symbol "("
              params <- var `sepBy` (symbol ",")
              symbol ")"
              body <- stmt
              symbol "endproc"
              return $ ProcedureStmt name params body

callStmt = do try $ symbol "call"
              name <- var
              symbol "("
              args <- expr `sepBy` (symbol ",")
              symbol ")"
              symbol ";"
              return $ CallStmt name args

seqStmt = do try $ symbol "do"
             stmts <- many1 stmt
             symbol "od"
             symbol ";"
             return $ SeqStmt stmts

stmt = quitStmt
   <|> printStmt
   <|> ifStmt
   <|> procStmt
   <|> callStmt
   <|> seqStmt
   <|> try setStmt
