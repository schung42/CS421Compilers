module Spec where

-- Version 1.4.2

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser)
import Data.List (isInfixOf)
import Main
import Tests


-- Equality of Exp and Val
-- -----------------------

-- We'll need equality over Exp for testing the parser
instance Eq Exp where
    IntExp i1 == IntExp i2  = i1 == i2
    SymExp s1 == SymExp s2  = s1 == s2
    SExp s1   == SExp   s2  = s1 == s2
    _         == _          = False

-- Run the parser/evaluator
-- ------------------------

-- Take a series of strings and convert them to expressions
genExps :: [String] -> [Exp]
genExps inputs = map pIn inputs
    where
        pIn input = case parse anExp "" input of
                        Left _  -> SymExp $ "Parse error on input: " ++ input
                        Right e -> e

-- Take a series of expressions as if we were a REPL
genVals :: [Exp] -> Env -> [Val]
genVals []     env  = []
genVals (e:es) env  = let evald = eval e env
                      in  evald : (genVals es $ case evald of
                                                DefVal k v  -> H.insert k v env
                                                _           -> env)

-- Defining/running tests
-- ----------------------

-- Pretty type for a TestCase (Input, ParseResult, EvaluationResultString)
type TestCase = ([String], [Exp], String)
-- and for a TestSuite (Name, Tests, Comments)
type TestSuite = (String, [TestCase], [String])

-- Run an individual tests
passesParser :: TestCase -> Bool
passesParser (input, exps, _) = genExps input == exps

passesEvaluator :: TestCase -> Bool
passesEvaluator (_, exps, val) = let result = show (genVals exps runtime)
                                 --in  result == val
                                 in  (result == val || bothHaveExn val result)
    where
        bothHaveExn v r =    "Exception" `isInfixOf` v
                          && "Exception" `isInfixOf` r

-- passesEvaluator (_, exps, val) = let result = show (genVals exps runtime)
--                                  in  result == val


-- Pretty failed results
prettyTestOut :: (TestCase -> (String, String)) -> TestCase -> [String]
prettyTestOut testOutput test@(input, _, _)
    = let   (shouldBe, actuallyIs) = testOutput test
      in    [ "#### Test case"
            , ""
            , "```"
            ] ++ input ++
            [ "```"
            , ""
            , "#### Should be:"
            , shouldBe
            , ""
            , "#### But actually is:"
            , actuallyIs
            , ""
            ]

-- Running test suites
testSuite :: ([TestCase] -> [TestCase]) -> (TestCase -> [String]) -> TestSuite -> [String]
testSuite filterer printerer (name, tests, comments)
    =   [ "TestSuite: " ++ name
        , "---------"
        , ""
        ]
        ++ case filterer tests of
                []      ->  [ "Passed!" ]
                failed  ->  [ "### General Comments"
                            ] ++ comments ++
                            [ ""
                            , "### Failed Tests"
                            , ""
                            ] ++ concatMap printerer failed
        ++ [ "" ]


-- Running Tests
-- -------------

allTests :: [TestSuite]
allTests =  [ basicIntsSyms
            , primitiveInts
            , primitiveBools
            , primitiveMisc
            , consListCarCdr
            , defDefineLambdaApply
            , allowsDefOverPrimForm
            , condForm
            , letForm
            , everythingElse
            , nestedQuasiQuotes
            ]

-- Do the run
runTests :: [TestSuite] -> IO ()
runTests tests = let parsdOut   = (\(input, exps, _) -> (show exps, show . genExps $ input))
                     evaldOut   = (\(_, exps, val) -> (val, show . flip genVals runtime $ exps))
                     failsParse = filter (not . passesParser)
                     failsEval  = filter (not . passesEvaluator)
                     parsdOuts  = concatMap (testSuite failsParse $ prettyTestOut parsdOut) tests
                     evaldOuts  = concatMap (testSuite failsEval $ prettyTestOut evaldOut) tests
                 in  mapM_ putStrLn $   [ ""
                                        , "Parser Tests"
                                        , "============"
                                        , ""
                                        ]
                                        ++ parsdOuts ++
                                        [ ""
                                        , ""
                                        , "Evaluator Tests"
                                        , "==============="
                                        , ""
                                        ]
                                        ++ evaldOuts


-- Generating Tests
-- ----------------

-- type TestCase = ([String], [Exp], String)
-- type TestSuite = (String, [TestCase], [String])
genTestSuiteOut :: (String, String, ([[String]], [String])) -> String
genTestSuiteOut (name, fncName, (tests, comments))
    = let parsd     = map genExps tests
          evald     = map (show . flip genVals runtime) parsd
          testCases = zip3 tests parsd evald
      in  fncName ++ " :: " ++ "TestSuite\n"
          ++ fncName ++ " = " ++    "("
                                    ++ "\"" ++ name ++ "\", "
                                    ++ show testCases ++
                                    ","
                                    ++ show comments ++
                                    ")"

genTestCase :: [String] -> TestCase
genTestCase t = let parsd = genExps t
                    evald = genVals parsd runtime
                in  (t, parsd, show evald)

testSuites :: [(String, String, ([[String]], [String]))]
testSuites =    [ ("Basic Integers/Symbols", "basicIntsSyms", basicIntsSymsOrig)
                , ("Primitive Integer Operators", "primitiveInts", primitiveIntsOrig)
                , ("Primitive Boolean Operators", "primitiveBools", primitiveBoolsOrig)
                , ("Primitive Misc.", "primitiveMisc", primitiveMiscOrig)
                , ("Cons, List, Car, Cdr", "consListCarCdr", consListCarCdrOrig)
                , ("Definition, Application", "defDefineLambdaApply", defDefineLambdaApplyOrig)
                , ("Overriding primitive/form Names", "allowsDefOverPrimForm", allowsDefOverPrimFormOrig)
                , ("`cond` form", "condForm", condFormOrig)
                , ("`let` form", "letForm", letFormOrig)
                , ("Everything else", "everythingElse", everythingElseOrig)
                , ("Nested Quasiquoting", "nestedQuasiQuotes", nestedQuasiQuotesOrig)
                ]

main :: IO ()
main = mapM_ (putStrLn . genTestSuiteOut) testSuites

-- Generated Tests
-- ---------------
basicIntsSyms :: TestSuite
basicIntsSyms = ("Basic Integers/Symbols", [(["435"],[IntExp 435],"[435]"),(["a"],[SymExp "a"],"[*** Scheme-Exception: Symbol a has no value. ***]"),(["a","555"],[SymExp "a",IntExp 555],"[*** Scheme-Exception: Symbol a has no value. ***,555]"),(["555","a"],[IntExp 555,SymExp "a"],"[555,*** Scheme-Exception: Symbol a has no value. ***]")],["Do you handle basic integer/symbol parsing correctly?","Do you generate an `ExnVal` on failed symbol lookup?","Do you continue evaluation in the `repl` even after a parse error?"])
primitiveInts :: TestSuite
primitiveInts = ("Primitive Integer Operators", [(["+","-","*"],[SymExp "+",SymExp "-",SymExp "*"],"[*primitive*,*primitive*,*primitive*]"),([">","<",">=","<=","=","!="],[SymExp ">",SymExp "<",SymExp ">=",SymExp "<=",SymExp "=",SymExp "!="],"[*primitive*,*primitive*,*primitive*,*primitive*,*primitive*,*primitive*]"),(["(+ 3 4 2 10)","(* 3 3 3 2)"],[SExp [SymExp "+",IntExp 3,IntExp 4,IntExp 2,IntExp 10],SExp [SymExp "*",IntExp 3,IntExp 3,IntExp 3,IntExp 2]],"[19,54]"),(["(+ 2 (* 3 4))","(- 20 1)","(- 10 5 2)","(- 10 (+ 4 5 (* 3 5)) (- 14 2) 22)"],[SExp [SymExp "+",IntExp 2,SExp [SymExp "*",IntExp 3,IntExp 4]],SExp [SymExp "-",IntExp 20,IntExp 1],SExp [SymExp "-",IntExp 10,IntExp 5,IntExp 2],SExp [SymExp "-",IntExp 10,SExp [SymExp "+",IntExp 4,IntExp 5,SExp [SymExp "*",IntExp 3,IntExp 5]],SExp [SymExp "-",IntExp 14,IntExp 2],IntExp 22]],"[14,19,3,-48]"),(["(> 5 3)","(> 6  4 2)","(> 6 4 2 6)"],[SExp [SymExp ">",IntExp 5,IntExp 3],SExp [SymExp ">",IntExp 6,IntExp 4,IntExp 2],SExp [SymExp ">",IntExp 6,IntExp 4,IntExp 2,IntExp 6]],"[t,t,nil]"),(["(>= 5 5 3)","(>= 5 6 3)","(<= 2 3 4)","(<= 2 2 3)","(<= 2 1 2)"],[SExp [SymExp ">=",IntExp 5,IntExp 5,IntExp 3],SExp [SymExp ">=",IntExp 5,IntExp 6,IntExp 3],SExp [SymExp "<=",IntExp 2,IntExp 3,IntExp 4],SExp [SymExp "<=",IntExp 2,IntExp 2,IntExp 3],SExp [SymExp "<=",IntExp 2,IntExp 1,IntExp 2]],"[t,nil,t,t,nil]"),(["(= 5 5 5 5)","(= 5 3 5 2)","(= 5 (+ 2 3) (- 8 3))","(= 4 2)","(= 4 (- 4 1) (+ 3 1))","(!= 1 2 1)","(!= 1 1 2)","(!= 22 22 22)","(!= (+ 1 22) 23 22)","(!= (+ 1 22) 24 26)"],[SExp [SymExp "=",IntExp 5,IntExp 5,IntExp 5,IntExp 5],SExp [SymExp "=",IntExp 5,IntExp 3,IntExp 5,IntExp 2],SExp [SymExp "=",IntExp 5,SExp [SymExp "+",IntExp 2,IntExp 3],SExp [SymExp "-",IntExp 8,IntExp 3]],SExp [SymExp "=",IntExp 4,IntExp 2],SExp [SymExp "=",IntExp 4,SExp [SymExp "-",IntExp 4,IntExp 1],SExp [SymExp "+",IntExp 3,IntExp 1]],SExp [SymExp "!=",IntExp 1,IntExp 2,IntExp 1],SExp [SymExp "!=",IntExp 1,IntExp 1,IntExp 2],SExp [SymExp "!=",IntExp 22,IntExp 22,IntExp 22],SExp [SymExp "!=",SExp [SymExp "+",IntExp 1,IntExp 22],IntExp 23,IntExp 22],SExp [SymExp "!=",SExp [SymExp "+",IntExp 1,IntExp 22],IntExp 24,IntExp 26]],"[t,nil,t,nil,nil,t,nil,nil,nil,t]")],["Do you print out `*primitive*` when a primitive operator is on its own?","Does your parer recursively handle nested expressions?","Do you handle subtraction (correctly) like this: (- 10 5 2) === (10 - 5) - 2 ?"])
primitiveBools :: TestSuite
primitiveBools = ("Primitive Boolean Operators", [(["and","or","not"],[SymExp "and",SymExp "or",SymExp "not"],"[*primitive*,*primitive*,*primitive*]"),(["(and 't 't)","(and 't 'nil)","(and 't 't 't 't)","(and 't 'nil 6 4)","(and 't 5)","(and (> 4 2) (> 5 2))","(and (> 4 2) (> 2 5))"],[SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "nil"],IntExp 6,IntExp 4],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],IntExp 5],SExp [SymExp "and",SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp ">",IntExp 5,IntExp 2]],SExp [SymExp "and",SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp ">",IntExp 2,IntExp 5]]],"[t,nil,t,nil,t,t,nil]"),(["(or 't 't)","(or 't 'nil)","(or 'nil 'nil)","(or 5 2 6)","(or (and 't 't) 'nil)","(or (and 'nil 't) 6)"],[SExp [SymExp "or",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "or",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "or",SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "or",IntExp 5,IntExp 2,IntExp 6],SExp [SymExp "or",SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "or",SExp [SymExp "and",SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "quote",SymExp "t"]],IntExp 6]],"[t,t,nil,t,t,t]"),(["(not (> 5 3))","(not (< 5 3))","(not 't 't)","(not 'nil 't 't 't)"],[SExp [SymExp "not",SExp [SymExp ">",IntExp 5,IntExp 3]],SExp [SymExp "not",SExp [SymExp "<",IntExp 5,IntExp 3]],SExp [SymExp "not",SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "not",SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "quote",SymExp "t"]]],"[nil,t,*** Scheme-Exception: `not` is a unary operator. ***,*** Scheme-Exception: `not` is a unary operator. ***]")],["Do you handle the boolean operators `and` and `or` correctly?","Do you handle the unary operator `not` correctl?"])
primitiveMisc :: TestSuite
primitiveMisc = ("Primitive Misc.", [(["()"],[SExp []],"[nil]"),(["eq?"],[SymExp "eq?"],"[*primitive*]"),(["(+)","(-)","(*)","(>)","(<)","(>=)","(<=)","(=)","(!=)","(eq?)","(and)","(or)","(not)","(list)","(car)","(cdr)"],[SExp [SymExp "+"],SExp [SymExp "-"],SExp [SymExp "*"],SExp [SymExp ">"],SExp [SymExp "<"],SExp [SymExp ">="],SExp [SymExp "<="],SExp [SymExp "="],SExp [SymExp "!="],SExp [SymExp "eq?"],SExp [SymExp "and"],SExp [SymExp "or"],SExp [SymExp "not"],SExp [SymExp "list"],SExp [SymExp "car"],SExp [SymExp "cdr"]],"[0,0,1,t,t,t,t,t,t,t,t,nil,*** Scheme-Exception: `not` is a unary operator. ***,nil,*** Scheme-Exception: `car` is a unary operator. ***,*** Scheme-Exception: `cdr` is a unary operator. ***]"),(["(+ 1)","(- 1)","(* 1)","(> 1)","(< 1)","(>= 1)","(<= 1)","(= 1)","(!= 1)","(eq? 'a)","(eq? 1)","(and 't)","(or 'nil)","(not 't)","(list 't)"],[SExp [SymExp "+",IntExp 1],SExp [SymExp "-",IntExp 1],SExp [SymExp "*",IntExp 1],SExp [SymExp ">",IntExp 1],SExp [SymExp "<",IntExp 1],SExp [SymExp ">=",IntExp 1],SExp [SymExp "<=",IntExp 1],SExp [SymExp "=",IntExp 1],SExp [SymExp "!=",IntExp 1],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"]],SExp [SymExp "eq?",IntExp 1],SExp [SymExp "and",SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "or",SExp [SymExp "quote",SymExp "nil"]],SExp [SymExp "not",SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "list",SExp [SymExp "quote",SymExp "t"]]],"[1,1,1,t,t,t,t,t,t,t,t,t,nil,nil,(t )]"),(["(eq? 5 5)","(eq? 6 5 6)","(eq? (+ 3 3) 6)","(eq? 'a 5)","(eq? 'a 'b)","(eq? 'a 'a 'b)","(eq? 'a 'a 'a)"],[SExp [SymExp "eq?",IntExp 5,IntExp 5],SExp [SymExp "eq?",IntExp 6,IntExp 5,IntExp 6],SExp [SymExp "eq?",SExp [SymExp "+",IntExp 3,IntExp 3],IntExp 6],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],IntExp 5],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]],SExp [SymExp "eq?",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"]]],"[t,nil,t,nil,nil,nil,t]")],["Do you handle the empty form `()` correctly?","Do you handle the base-cases of the primitive operators correctly?","Do you handle `eq?` correctly?","Do you handle singleton cases properly for primitives (single argument)?"])
consListCarCdr :: TestSuite
consListCarCdr = ("Cons, List, Car, Cdr", [(["cons","list","car","cdr"],[SymExp "cons",SymExp "list",SymExp "car",SymExp "cdr"],"[*** Scheme-Exception: Symbol cons has no value. ***,*primitive*,*primitive*,*primitive*]"),(["(list (> 3 4) 't 15 'nil (< 5 2 3 5))","(car (list 'a 'b))","(cdr (list 'a 'b))","(car (list 'a 'b 'c))","(cdr (list 'a 'b 'c))","(cdr (list 'a))","(cdr 'a)"],[SExp [SymExp "list",SExp [SymExp ">",IntExp 3,IntExp 4],SExp [SymExp "quote",SymExp "t"],IntExp 15,SExp [SymExp "quote",SymExp "nil"],SExp [SymExp "<",IntExp 5,IntExp 2,IntExp 3,IntExp 5]],SExp [SymExp "car",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "car",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"],SExp [SymExp "quote",SymExp "c"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"],SExp [SymExp "quote",SymExp "c"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "a"]]],SExp [SymExp "cdr",SExp [SymExp "quote",SymExp "a"]]],"[(nil t 15 nil nil ),a,(b ),a,(b c ),nil,*** Scheme-Exception: Not a cons cell: a ***]"),(["(cons 2 3)"],[SExp [SymExp "cons",IntExp 2,IntExp 3]],"[(2 . 3)]"),(["(cons 2 (cons 3 4))","(cons 2 (cons 3 (cons 4 'nil)))"],[SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,IntExp 4]],SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,SExp [SymExp "cons",IntExp 4,SExp [SymExp "quote",SymExp "nil"]]]]],"[(2 3 . 4),(2 3 4 )]"),(["(car (cons 2 (cons 3 (cons 4 'nil))))","(cdr (cons 2 (cons 3 (cons 4 'nil))))","(car (list 2 3 4 'a 'b))","(cdr (list 'b 'c 3 4 (+ 3 4)))"],[SExp [SymExp "car",SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,SExp [SymExp "cons",IntExp 4,SExp [SymExp "quote",SymExp "nil"]]]]],SExp [SymExp "cdr",SExp [SymExp "cons",IntExp 2,SExp [SymExp "cons",IntExp 3,SExp [SymExp "cons",IntExp 4,SExp [SymExp "quote",SymExp "nil"]]]]],SExp [SymExp "car",SExp [SymExp "list",IntExp 2,IntExp 3,IntExp 4,SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cdr",SExp [SymExp "list",SExp [SymExp "quote",SymExp "b"],SExp [SymExp "quote",SymExp "c"],IntExp 3,IntExp 4,SExp [SymExp "+",IntExp 3,IntExp 4]]]],"[2,(3 4 ),2,(c 3 4 7 )]"),(["(car (cons 1 2) (cons 2 3))","(cdr (cons 2 't) (list 4 2 4 5))"],[SExp [SymExp "car",SExp [SymExp "cons",IntExp 1,IntExp 2],SExp [SymExp "cons",IntExp 2,IntExp 3]],SExp [SymExp "cdr",SExp [SymExp "cons",IntExp 2,SExp [SymExp "quote",SymExp "t"]],SExp [SymExp "list",IntExp 4,IntExp 2,IntExp 4,IntExp 5]]],"[*** Scheme-Exception: `car` is a unary operator. ***,*** Scheme-Exception: `cdr` is a unary operator. ***]")],["Are you handling primitives `list`, `car`, and `cdr` correctly?","Are you handling the `cons` form correctly?","Do you produce an `ExnVal` if `car` or `cdr` is applied to too many arguments?"])
defDefineLambdaApply :: TestSuite
defDefineLambdaApply = ("Definition, Application", [(["(def x (+ 10 20))","x","y"],[SExp [SymExp "def",SymExp "x",SExp [SymExp "+",IntExp 10,IntExp 20]],SymExp "x",SymExp "y"],"[x,30,*** Scheme-Exception: Symbol y has no value. ***]"),(["(def x 1)","(define inc (y) (+ y x))","(inc 10)","(def x 2)","(inc 10)"],[SExp [SymExp "def",SymExp "x",IntExp 1],SExp [SymExp "define",SymExp "inc",SExp [SymExp "y"],SExp [SymExp "+",SymExp "y",SymExp "x"]],SExp [SymExp "inc",IntExp 10],SExp [SymExp "def",SymExp "x",IntExp 2],SExp [SymExp "inc",IntExp 10]],"[x,inc,11,x,11]"),(["(lambda (x) (+ x 10))","( (lambda (x) (+ x 10)) 20)","(define mkInc (x) (lambda (y) (+ x y)))","(def i2 (mkInc 2))","(i2 10)"],[SExp [SymExp "lambda",SExp [SymExp "x"],SExp [SymExp "+",SymExp "x",IntExp 10]],SExp [SExp [SymExp "lambda",SExp [SymExp "x"],SExp [SymExp "+",SymExp "x",IntExp 10]],IntExp 20],SExp [SymExp "define",SymExp "mkInc",SExp [SymExp "x"],SExp [SymExp "lambda",SExp [SymExp "y"],SExp [SymExp "+",SymExp "x",SymExp "y"]]],SExp [SymExp "def",SymExp "i2",SExp [SymExp "mkInc",IntExp 2]],SExp [SymExp "i2",IntExp 10]],"[*closure*,30,mkInc,i2,12]"),(["(define f ((+ 3 4) x y) (+ x y))","(def (+ 3 4) 5)"],[SExp [SymExp "define",SymExp "f",SExp [SExp [SymExp "+",IntExp 3,IntExp 4],SymExp "x",SymExp "y"],SExp [SymExp "+",SymExp "x",SymExp "y"]],SExp [SymExp "def",SExp [SymExp "+",IntExp 3,IntExp 4],IntExp 5]],"[*** Scheme-Exception: Must use only `SymExp` for parameter names. ***,*** Scheme-Exception: Symbol def has no value. ***]"),(["(define f (x y z) (+ (- x y) z))","(f 3 4 2 8 0)"],[SExp [SymExp "define",SymExp "f",SExp [SymExp "x",SymExp "y",SymExp "z"],SExp [SymExp "+",SExp [SymExp "-",SymExp "x",SymExp "y"],SymExp "z"]],SExp [SymExp "f",IntExp 3,IntExp 4,IntExp 2,IntExp 8,IntExp 0]],"[f,1]")],["Do you handle constant definition correctly?","Do you handle function definition/closure application correctly?","Do you handle printing a `Closure` as `*closure*`?","Do you handle `def` and `define` having non-symbol parameter names?","For a `Closure` applied to extra arguments, are the extra ones thrown away?"])
allowsDefOverPrimForm :: TestSuite
allowsDefOverPrimForm = ("Overriding primitive/form Names", [(["(def define 3)","define"],[SExp [SymExp "def",SymExp "define",IntExp 3],SymExp "define"],"[define,3]"),(["(define let (x y z) (+ x y z))","(let 3 4 5)","(let ((x 5) (y 100) (z 3)) (+ x (- y z)))"],[SExp [SymExp "define",SymExp "let",SExp [SymExp "x",SymExp "y",SymExp "z"],SExp [SymExp "+",SymExp "x",SymExp "y",SymExp "z"]],SExp [SymExp "let",IntExp 3,IntExp 4,IntExp 5],SExp [SymExp "let",SExp [SExp [SymExp "x",IntExp 5],SExp [SymExp "y",IntExp 100],SExp [SymExp "z",IntExp 3]],SExp [SymExp "+",SymExp "x",SExp [SymExp "-",SymExp "y",SymExp "z"]]]],"[let,12,102]"),(["(def + 3)","(- + 2)","(* + 10)"],[SExp [SymExp "def",SymExp "+",IntExp 3],SExp [SymExp "-",SymExp "+",IntExp 2],SExp [SymExp "*",SymExp "+",IntExp 10]],"[+,1,30]"),(["(define quote (x y) (list x y))","(quote 'a 'b)","(quote 'a)"],[SExp [SymExp "define",SymExp "quote",SExp [SymExp "x",SymExp "y"],SExp [SymExp "list",SymExp "x",SymExp "y"]],SExp [SymExp "quote",SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "b"]],SExp [SymExp "quote",SExp [SymExp "quote",SymExp "a"]]],"[quote,(a b ),(quote a )]")],["Do you allow users to `define` or `def` variables with the same name","as primitive operators? If you redefine a form (such as `let` or `quote`)","as an operator, but it's used correctly as a form, the definition as a","form should be used. Otherwise the environment lookup should be used."])
condForm :: TestSuite
condForm = ("`cond` form", [(["(cond ((> 4 3) 'a (> 4 2) 'b))","(cond ((< 4 3) 'a (> 4 2) 'b))","(cond ((< 4 3) 'a (< 4 2) 'b))"],[SExp [SymExp "cond",SExp [SExp [SymExp ">",IntExp 4,IntExp 3],SExp [SymExp "quote",SymExp "a"],SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cond",SExp [SExp [SymExp "<",IntExp 4,IntExp 3],SExp [SymExp "quote",SymExp "a"],SExp [SymExp ">",IntExp 4,IntExp 2],SExp [SymExp "quote",SymExp "b"]]],SExp [SymExp "cond",SExp [SExp [SymExp "<",IntExp 4,IntExp 3],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "<",IntExp 4,IntExp 2],SExp [SymExp "quote",SymExp "b"]]]],"[a,b,nil]")],["Do you handle the `cond` form correctly in `eval`?"])
letForm :: TestSuite
letForm = ("`let` form", [(["(let ((x 5) (y 10)) (+ x y))","(def x 20)","(def y 30)","(let ((x 11) (y 4)) (- (* x y) 2))","x","y"],[SExp [SymExp "let",SExp [SExp [SymExp "x",IntExp 5],SExp [SymExp "y",IntExp 10]],SExp [SymExp "+",SymExp "x",SymExp "y"]],SExp [SymExp "def",SymExp "x",IntExp 20],SExp [SymExp "def",SymExp "y",IntExp 30],SExp [SymExp "let",SExp [SExp [SymExp "x",IntExp 11],SExp [SymExp "y",IntExp 4]],SExp [SymExp "-",SExp [SymExp "*",SymExp "x",SymExp "y"],IntExp 2]],SymExp "x",SymExp "y"],"[15,x,y,42,20,30]")],["Do you handle the `let` form correctly in `eval`?"])
everythingElse :: TestSuite
everythingElse = ("Everything else", [(["435"],[IntExp 435],"[435]"),(["(def x 5)","x","y"],[SExp [SymExp "def",SymExp "x",IntExp 5],SymExp "x",SymExp "y"],"[x,5,*** Scheme-Exception: Symbol y has no value. ***]"),(["(f 10 30 x)"],[SExp [SymExp "f",IntExp 10,IntExp 30,SymExp "x"]],"[*** Scheme-Exception: Symbol f has no value. ***]"),(["+"],[SymExp "+"],"[*primitive*]"),(["()"],[SExp []],"[nil]"),(["'a","'5","(quote a)","'a","'asdf","'*first-val*"],[SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",IntExp 5],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SymExp "asdf"],SExp [SymExp "quote",SymExp "*first-val*"]],"[a,5,a,a,asdf,*first-val*]"),(["(define fact (n) (cond ((< n 1) 1 't (* n (fact (- n 1))))))","(fact 5)"],[SExp [SymExp "define",SymExp "fact",SExp [SymExp "n"],SExp [SymExp "cond",SExp [SExp [SymExp "<",SymExp "n",IntExp 1],IntExp 1,SExp [SymExp "quote",SymExp "t"],SExp [SymExp "*",SymExp "n",SExp [SymExp "fact",SExp [SymExp "-",SymExp "n",IntExp 1]]]]]],SExp [SymExp "fact",IntExp 5]],"[fact,120]"),(["'a","''a","(car (quote (a b c)))","(car '(a b c))","(car ''(a b c))","'(2 3 4)","(list (+ 2 3))","'( (+ 2 3))","'(+ 2 3)"],[SExp [SymExp "quote",SymExp "a"],SExp [SymExp "quote",SExp [SymExp "quote",SymExp "a"]],SExp [SymExp "car",SExp [SymExp "quote",SExp [SymExp "a",SymExp "b",SymExp "c"]]],SExp [SymExp "car",SExp [SymExp "quote",SExp [SymExp "a",SymExp "b",SymExp "c"]]],SExp [SymExp "car",SExp [SymExp "quote",SExp [SymExp "quote",SExp [SymExp "a",SymExp "b",SymExp "c"]]]],SExp [SymExp "quote",SExp [IntExp 2,IntExp 3,IntExp 4]],SExp [SymExp "list",SExp [SymExp "+",IntExp 2,IntExp 3]],SExp [SymExp "quote",SExp [SExp [SymExp "+",IntExp 2,IntExp 3]]],SExp [SymExp "quote",SExp [SymExp "+",IntExp 2,IntExp 3]]],"[a,(quote a ),a,a,quote,(2 3 4 ),(5 ),((+ 2 3 ) ),(+ 2 3 )]"),(["'(+ 1 2)","(eval '(+ 1 2))","(eval ''(+ 1 2))","(eval (eval ''(+ 1 2)))","(def a '(+ x 1))","(def x 5)","(eval a)"],[SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]],SExp [SymExp "eval",SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]]],SExp [SymExp "eval",SExp [SymExp "quote",SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]]]],SExp [SymExp "eval",SExp [SymExp "eval",SExp [SymExp "quote",SExp [SymExp "quote",SExp [SymExp "+",IntExp 1,IntExp 2]]]]],SExp [SymExp "def",SymExp "a",SExp [SymExp "quote",SExp [SymExp "+",SymExp "x",IntExp 1]]],SExp [SymExp "def",SymExp "x",IntExp 5],SExp [SymExp "eval",SymExp "a"]],"[(+ 1 2 ),3,(+ 1 2 ),3,a,x,6]"),(["(def a 5)","`(+ a 1)","`(+ ,a 1)"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "+",SymExp "a",IntExp 1]],SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SymExp "a"],IntExp 1]]],"[a,(+ a 1 ),(+ 5 1 )]"),(["(defmacro if (con then else) `(cond (,con ,then 't ,else)))","(def a 5)","(if (> a 2) 10 20)","(if (< a 2) 10 20)","(define fact (n) (if (< n 1) 1 (* n (fact (- n 1)))))","(fact 10)","(defmacro mkplus (e) (if (eq? (car e) '-) (cons '+ (cdr e)) e))","(mkplus (- 5 4))"],[SExp [SymExp "defmacro",SymExp "if",SExp [SymExp "con",SymExp "then",SymExp "else"],SExp [SymExp "quasiquote",SExp [SymExp "cond",SExp [SExp [SymExp "unquote",SymExp "con"],SExp [SymExp "unquote",SymExp "then"],SExp [SymExp "quote",SymExp "t"],SExp [SymExp "unquote",SymExp "else"]]]]],SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "if",SExp [SymExp ">",SymExp "a",IntExp 2],IntExp 10,IntExp 20],SExp [SymExp "if",SExp [SymExp "<",SymExp "a",IntExp 2],IntExp 10,IntExp 20],SExp [SymExp "define",SymExp "fact",SExp [SymExp "n"],SExp [SymExp "if",SExp [SymExp "<",SymExp "n",IntExp 1],IntExp 1,SExp [SymExp "*",SymExp "n",SExp [SymExp "fact",SExp [SymExp "-",SymExp "n",IntExp 1]]]]],SExp [SymExp "fact",IntExp 10],SExp [SymExp "defmacro",SymExp "mkplus",SExp [SymExp "e"],SExp [SymExp "if",SExp [SymExp "eq?",SExp [SymExp "car",SymExp "e"],SExp [SymExp "quote",SymExp "-"]],SExp [SymExp "cons",SExp [SymExp "quote",SymExp "+"],SExp [SymExp "cdr",SymExp "e"]],SymExp "e"]],SExp [SymExp "mkplus",SExp [SymExp "-",IntExp 5,IntExp 4]]],"[if,a,10,20,fact,3628800,mkplus,9]")],["Do you handle quotes, unquotes, quasiquotes, and macros correctly?"])
nestedQuasiQuotes :: TestSuite
nestedQuasiQuotes = ("Nested Quasiquoting", [(["(def a 5)","``(+ ,,a 1)","``(+ ,,a ,a)","`(+ a ,,a)","``(+ a ,,a)","(eval ``(+ ,,a 1))","(eval (eval ``(+ ,,a 1)))"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],IntExp 1]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],SExp [SymExp "unquote",SymExp "a"]]]],SExp [SymExp "quasiquote",SExp [SymExp "+",SymExp "a",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SymExp "a",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]],SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],IntExp 1]]]],SExp [SymExp "eval",SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]],IntExp 1]]]]]],"[a,(quasiquote (+ (unquote 5 ) 1 ) ),(quasiquote (+ (unquote 5 ) (unquote a ) ) ),(+ a *** Scheme-Exception: Cannot `unquote` more than `quasiquote`. *** ),(quasiquote (+ a (unquote 5 ) ) ),(+ 5 1 ),6]"),(["(def a 5)","```(+ ,,,a ,,a)","```(+ ,a ,,a)","```(+ `a `(+ ,,,,a a))"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]],SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SymExp "a"],SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "quasiquote",SymExp "a"],SExp [SymExp "quasiquote",SExp [SymExp "+",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]],SymExp "a"]]]]]]],"[a,(quasiquote (quasiquote (+ (unquote (unquote 5 ) ) (unquote (unquote a ) ) ) ) ),(quasiquote (quasiquote (+ (unquote a ) (unquote (unquote a ) ) ) ) ),(quasiquote (quasiquote (+ (quasiquote a ) (quasiquote (+ (unquote (unquote (unquote 5 ) ) ) a ) ) ) ) )]"),(["(def a 5)","```a","```,a","```,,a","```,,,a","(eval ```,,,a)","(eval (eval ```,,,a))"],[SExp [SymExp "def",SymExp "a",IntExp 5],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SymExp "a"]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SymExp "a"]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]],SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]],SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]]],SExp [SymExp "eval",SExp [SymExp "eval",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "quasiquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SExp [SymExp "unquote",SymExp "a"]]]]]]]]],"[a,(quasiquote (quasiquote a ) ),(quasiquote (quasiquote (unquote a ) ) ),(quasiquote (quasiquote (unquote (unquote a ) ) ) ),(quasiquote (quasiquote (unquote (unquote 5 ) ) ) ),(quasiquote (unquote 5 ) ),5]")],["Do you handle nested quasiquotes?"])

