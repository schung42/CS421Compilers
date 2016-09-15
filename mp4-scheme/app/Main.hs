module Main where

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim
import Data.Functor.Identity
import qualified Data.HashMap.Strict as H

fixMe = undefined


-- Datatypes
-- ---------

-- Exp data type (hold parse results before evaluation)
data Exp = IntExp Integer
         | SymExp String
         deriving (Show)

-- Val data type (for results of evaluation)
data Val = IntVal Integer
         | SymVal String


-- Parsers
-- -------

-- Pretty name for Parser types
type Parser = ParsecT String () Identity

-- Lexicals

adigit :: Parser Char
adigit = oneOf ['0'..'9']

digits :: Parser String
digits = many1 adigit

-- Remember to define this parser. You should handle spaces, tabs, and newlines.
whitespace :: Parser String
whitespace = fixMe

-- Grammaticals

anInt :: Parser Exp
anInt = do  d <- digits
            return $ IntExp (read d)

-- Remember to modify `anExp` as to make it aware of Exp parsers you add
anExp :: Parser Exp
anExp = anInt


-- Environment
-- -----------

-- Lift/Lower Haskell Bool to/from a boolean value in our Scheme representation
liftbool :: Bool -> Val
liftbool False = SymVal "nil"
liftbool True  = SymVal "t"

lowerbool :: Val -> Bool
lowerbool (SymVal "nil") = False
lowerbool _              = True

-- Lift/Lower a Haskell Int to/from an int value in our Scheme representation
liftint :: Integer -> Val
liftint = IntVal

lowerint :: Val -> Integer
lowerint (IntVal i) = i
lowerint _          = error "Cannot lower, not an IntVal!"

-- These are helpers which take a Haskell operator (like `(+)`), a base-case
-- (what's the base-case for `(+)`?), and turn them into a suitable `PrimVal` to
-- be used by our Scheme.
liftIntOp :: (Integer -> Integer -> Integer) -> Integer -> Val
liftIntOp f z = fixMe

liftIntBoolOp :: (Integer -> Integer -> Bool) -> Val
liftIntBoolOp f = fixMe

liftBoolOp :: (Bool -> Bool -> Bool) -> Val
liftBoolOp f = fixMe

-- Pretty name for the Env type
type Env = H.HashMap String Val

-- The `runtime` is the initial environment for evaluation. Primitive operators
-- such as "+", "eq?", and "cdr" must be inserted into this runtime.
runtime :: Env
runtime = H.fromList    [ ("+", liftIntOp (+) 0)
                        , ("-", fixMe)
                        ]

-- Evaluation
-- ----------

-- Helpers

-- This `eval` must handle every way an `Exp` could be constructed.
eval :: Exp -> Env -> Val
eval (IntExp i) env                                                     --- integers
    = IntVal i


-- Printing
-- --------

-- This `show` must handle every way a `Val` could be constructed.
instance Show Val where
    -- show :: Val -> String
    show (IntVal i)         = show i


-- REPL
-- ----

-- The line with `EVAL, PRINT` is where valid parses make it through to.
-- Remember to modify this so that the result of `eval` is checked. There is
-- special behavior in the REPL if the evaluation returns a `DefVal`
repl :: Env -> IO ()
repl env =
    do  putStr "scheme> "
        l <- getLine                                                        -- READ
        case parse anExp "Expression" l of                                  -- READ
            Right exp -> putStrLn $ show (eval exp env)                     -- EVAL, PRINT
            Left pe   -> putStrLn (show pe)                                 -- PRINT
        repl env                                                            -- LOOP
