import Autograder
import Control.Monad
import Data.List
import Lib
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Tests
import qualified Continuation as C

main = do {
          ; putStrLn ""
          ; testProp (prop_factk, "factk", factTests)
          ; testProp (prop_evenoddk, "evenoddk", evenoddTests)
          ; testProp (prop_isSimple, "isSimple", isSimpleTests)
          ; testCases (evalStmt, "Given Cases", cpsDeclTests)
          ; testCases (evalStmt, "Student Written Cases", studentCpsDeclTests)
          ; xs <- (sample' (stmt' 100))
          ; putStrLn ""
          ; putStrLn "\nAnd some test cases for you..."
          ; mapM_ (\x -> do { putStr "Test: "
                                      ; print x
                                      ; putStr "\t"
                                      ; print (C.cpsDecl x)
                                      ; putStr "\n"
                  })
                  xs
          }

{- DO NOT MODIFY BELOW THIS LINE! -}

cpsDeclTests = [
                -- The Basics
                ("foo v = v", Decl "fook" ["v","k"] (AppExp (VarExp "k") (VarExp "v"))), ("foo v = 5", Decl "fook" ["v","k"] (AppExp (VarExp "k") (IntExp 5))),

                -- Application
                ("foo f x = f x", Decl "fook" ["f","x","k"] (AppExp (AppExp (VarExp "f") (VarExp "x")) (VarExp "k"))),
                -- If Expressions
                ("foo f x = if x then 1 else 0", Decl "fook" ["f","x","k"] (IfExp (VarExp "x") (AppExp (VarExp "k") (IntExp 1)) (AppExp (VarExp "k") (IntExp 0)))),
                ("foo f x = if f x then 1 else 0", Decl "fook" ["f","x","k"] (AppExp (AppExp (VarExp "f") (VarExp "x")) (LamExp "v1" (IfExp (VarExp "v1") (AppExp (VarExp "k") (IntExp 1)) (AppExp (VarExp "k") (IntExp 0)))))),

                -- Operator Expressions
                ("foo x y = x + y", Decl "fook" ["x","y","k"] (AppExp (VarExp "k") (OpExp "+" (VarExp "x") (VarExp "y")))),
                ("foo f x y = f x + y", Decl "fook" ["f","x","y","k"] (AppExp (AppExp (VarExp "f") (VarExp "x")) (LamExp "v1" (AppExp (VarExp "k") (OpExp "+" (VarExp "v1") (VarExp "y")))))),
                ("foo f x y = x + f y", Decl "fook" ["f","x","y","k"] (AppExp (AppExp (VarExp "f") (VarExp "y")) (LamExp "v1" (AppExp (VarExp "k") (OpExp "+" (VarExp "x") (VarExp "v1")))))),
                ("foo f g x y = f x + g y", Decl "fook" ["f","g","x","y","k"] (AppExp (AppExp (VarExp "f") (VarExp "x")) (LamExp "v1" (AppExp (AppExp (VarExp "g") (VarExp "y")) (LamExp "v2" (AppExp (VarExp "k") (OpExp "+" (VarExp "v1") (VarExp "v2")))))))),

                -- Potpourri
                ("foo x = if x > 5 then 1 else 0", Decl "fook" ["x","k"] (IfExp (OpExp ">" (VarExp "x") (IntExp 5)) (AppExp (VarExp "k") (IntExp 1)) (AppExp (VarExp "k") (IntExp 0)))),
                ("foo f x = if f x > 5 then 1 else 0", Decl "fook" ["f","x","k"] (AppExp (AppExp (VarExp "f") (VarExp "x")) (LamExp "v2" (AppExp (LamExp "v1" (IfExp (VarExp "v1") (AppExp (VarExp "k") (IntExp 1)) (AppExp (VarExp "k") (IntExp 0)))) (OpExp ">" (VarExp "v2") (IntExp 5)))))),
                ("ifapp f g h = (if (g > h) then f (g + 5) else f (h + 5))", Decl "ifappk" ["f","g","h","k"] (IfExp (OpExp ">" (VarExp "g") (VarExp "h")) (AppExp (AppExp (VarExp "f") (OpExp "+" (VarExp "g") (IntExp 5))) (VarExp "k")) (AppExp (AppExp (VarExp "f") (OpExp "+" (VarExp "h") (IntExp 5))) (VarExp "k"))))
               ]

evalStmt input = case parseDecl input of
    Right x -> C.cpsDecl x
    Left x -> error "fail"

instance Arbitrary Stmt where
  arbitrary = sized stmt'

stmt' n = do { expr'' <- (exp' [[x] | x <- (['a' .. 'j'] ++ ['l' .. 'z'])])
             ; params <- shuffle (gatherVars expr'')
             ; fname <- listOf1 (elements (['a' .. 'j'] ++ ['l' .. 'z']))
             ; return (Decl fname params expr'')
             }

gatherVars (VarExp x) = [x]
gatherVars (AppExp e1 e2) = gatherVars e1 ++ gatherVars e2
gatherVars (IfExp e1 e2 e3) = gatherVars e1 ++ gatherVars e2 ++ gatherVars e3
gatherVars (OpExp _ e1 e2) = gatherVars e1 ++ gatherVars e2
gatherVars _ = []

instance Arbitrary Exp where
  arbitrary = exp' alphastring
    where
      alphastring = (map (\x -> [x]) ['a' .. 'z'])

exp' alphastring = frequency [
         (10, liftM VarExp (elements alphastring)),
         (10, liftM IntExp (choose (1,1000000))),
         (1, liftM2 AppExp sub sub),
         (5, liftM3 IfExp sub sub sub),
         (5, liftM3 OpExp op sub sub)
         --(1, liftM2 LamExp (elements alphastring) sub)
       ]
 where
   sub = exp' alphastring
   op = elements ["*", "/", "+", "-", "<", ">", "==", "<=", ">="]


{----------------------------------
 - Tests for factk
 ----------------------------------}

factTests = map Positive factTests'

factorial 0 = 1
factorial n = factorial (n-1) * n

prop_factk :: Positive Integer -> Bool
prop_factk (Positive x) = (C.factk x (\y -> y)) == factorial x

{----------------------------------
 - Tests for evenodd
 ----------------------------------}

evenoddTests = map (NonEmpty) evenoddTests'

prop_evenoddk :: NonEmptyList Int -> Bool
prop_evenoddk (NonEmpty xs) = (C.evenoddk xs id id) == (if lastElem == 0 then resultEven else resultOdd)
  where lastElem = mod (xs !! (length xs - 1)) 2
        resultEven = sum (filter even xs)
        resultOdd = sum (filter odd xs)

{----------------------------------
 - Tests for isSimple
 ----------------------------------}

prop_isSimple :: Exp -> Bool
prop_isSimple x = (not (isInfixOf "LamExp" x') && (C.isSimple x == not (isInfixOf "AppExp" x'))) || ((isInfixOf "LamExp" x'))
  where
    x' = show x
