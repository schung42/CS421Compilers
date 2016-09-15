module Tests where
import Lib

factTests' = [
              10
            ] :: [Integer]

evenoddTests' = [
                 [1]
               , [1,2]
               ] :: [[Int]]

isSimpleTests = [
                ]

studentCpsDeclTests = [
                 ("foo v = v", Decl "fook" ["v","k"] (AppExp (VarExp "k") (VarExp "v")))
               ] :: [(String, Stmt)]
