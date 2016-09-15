import Autograder
import Control.Exception.Base
import Control.Monad
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import Lib
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Tests
import qualified Unifier as U

instance Arbitrary TyCon where
  arbitrary = tycon' 1

tycon' n = frequency [
         (6, elements [IntTy, StringTy, BoolTy]),
         (12, elements [TyVar n]),
         (8, liftM2 PairTy tycon1 tycon2),
         (8, liftM2 FnTy tycon1 tycon2),
         (12, liftM ListTy tycon1)
       ]
  where
        tycon1 = tycon' n
        tycon2 = tycon' (n+1)

prop_substFun :: SubstEnv -> TyCon -> Bool
prop_substFun env t@(TyVar n) = if not (H.member n env)
                                then ("v" ++ (show n)) == studentAnswer
                                else show (env H.! n) == studentAnswer
                                where studentAnswer = show $ U.substFun env t
prop_substFun _ _ = True

prop_monoTyLiftSubst :: SubstEnv -> TyCon -> Bool
prop_monoTyLiftSubst env tycon = dasolution' == studentAnswer
                                 where studentAnswer = show $ U.monoTyLiftSubst env tycon
                                       dasolution' = sillyMonoTyLiftSubst env tycon

sillyMonoTyLiftSubst :: SubstEnv -> TyCon -> String
sillyMonoTyLiftSubst env tycon = dasolution'
                                 where
                                   tyconString = T.pack (show tycon)
                                   dasolution1 = T.unpack (foldl (\acc -> \(find, rep) -> T.replace (T.pack $ ("v" ++ show find ++ ")")) (T.pack $ show rep ++ ")") acc) tyconString (H.toList env))
                                   dasolution2 = T.unpack (foldl (\acc -> \(find, rep) -> T.replace (T.pack $ ("v" ++ show find ++ "]")) (T.pack $ show rep ++ "]") acc) (T.pack dasolution1) (H.toList env))
                                   dasolution3 = T.unpack (foldl (\acc -> \(find, rep) -> T.replace (T.pack $ ("v" ++ show find ++ ",")) (T.pack $ show rep ++ ",") acc) (T.pack dasolution2) (H.toList env))
                                   dasolution4 = T.unpack (foldl (\acc -> \(find, rep) -> T.replace (T.pack $ ("v" ++ show find ++ " ")) (T.pack $ show rep ++ " ") acc) (T.pack dasolution3) (H.toList env))
                                   dasolution' = T.unpack (foldl (\acc -> \(find, rep) -> if T.isSuffixOf (T.pack $ "v" ++ show find) acc then T.append (T.dropEnd (T.length (T.pack $ "v" ++ show find)) acc) (T.pack $ show rep) else acc) (T.pack dasolution4) (H.toList env))

prop_occurs :: TyCon -> TyCon -> Bool
prop_occurs (PairTy a b) h = prop_occurs a h && prop_occurs b h
prop_occurs (FnTy a b) h = prop_occurs a h && prop_occurs b h
prop_occurs (ListTy a) h = prop_occurs a h
prop_occurs n@(TyVar a) haystack = lesolution == studentAnswer
                                   where studentAnswer = U.occurs n haystack
                                         lesolution = sillyOccurs n haystack
prop_occurs _ _ = True -- default to true for non-tyvars

sillyOccurs n@(TyVar a) haystack = sillyOccurs' n (show haystack)
sillyOccurs' n@(TyVar a) haystack = lesolution
                                   where vNumber = show n
                                         showHaystack = haystack
                                         lesolution = L.isInfixOf (vNumber ++ ")") showHaystack ||
                                                      L.isInfixOf (vNumber ++ "]") showHaystack ||
                                                      L.isInfixOf (vNumber ++ ",") showHaystack ||
                                                      L.isInfixOf (vNumber ++ " ") showHaystack ||
                                                      L.isSuffixOf vNumber showHaystack

listify :: String -> String
listify str = "[" ++ str ++ "]"

-- original eqn set, composition of replaces, results to concat, the result
sillyUnify :: EqnSet -> EqnSet -> String
sillyUnify [] eqns = if result == "Nothing" then "Nothing" else listify $ result
  where result = sillySub (map (\(x, y) -> (x, show y)) eqns) eqns []
sillyUnify (((TyVar a),(TyVar b)):xs) eqns
  | a == b = sillyUnify xs eqns -- delete
  | otherwise = sillyUnify xs (eqns ++ [(TyVar a, TyVar b)]) -- eliminate for tyvars
sillyUnify ((a@(TyVar _), b):xs) eqns
  | (not $ sillyOccurs a b) = sillyUnify xs (eqns ++ [(a,b)])-- part 1 of elim
  | otherwise = "Nothing"
-- at this point s is not a variable
sillyUnify ((a, b@(TyVar _)):xs) eqns = sillyUnify ((b,a):xs) eqns-- orient
sillyUnify ((a, b):xs) eqns
  | a == b = sillyUnify xs eqns -- delete
  | [] <- sillyUnwrap a b = "Nothing"
  | ys <- sillyUnwrap a b = sillyUnify (ys ++ xs) eqns-- decompose
  | otherwise = "Nothing"

sillySub :: [(TyCon, String)] -> EqnSet -> [String] -> String
sillySub [] ys strs = L.intercalate "," strs
sillySub zs@((p@(TyVar n),q):xs) ys strs
  | (show p) == q = sillySub xs ys strs
  | otherwise = if (length (filter (\(x,y) -> sillyOccurs' x q) (filter (\(x,y) -> sillyOccurs' p y) xs')) > 0) ||
                   (sillyOccurs p q)
                then "Nothing"
                else sillySub xs' ys (strs ++ [result])
                     where xs' = map (\(x,y) -> if sillyOccurs' p y then (x, sillyReplace p q y) else (x,y)) xs
                           q' = fixPt q
                           f = (\str -> foldl (\z -> \(x, y) -> sillyReplace x y z) str xs')
                           fixPt x = if x == f x then x else fixPt (f x)
                           result = ("(" ++ show n ++ "," ++ q' ++ ")")

sillyReplace (TyVar p) replaceWith inString = soln
  where packRep = T.pack inString
        packQ = T.pack replaceWith
        rep1 = T.replace (T.pack $ "v" ++ show p ++ ")") (T.append packQ (T.pack ")")) packRep
        rep2 = T.replace (T.pack $ "v" ++ show p ++ "]") (T.append packQ (T.pack "]")) rep1
        rep3 = T.replace (T.pack $ "v" ++ show p ++ ",") (T.append packQ (T.pack ",")) rep2
        rep4 = T.replace (T.pack $ "v" ++ show p ++ " ") (T.append packQ (T.pack " ")) rep3
        rep5 = if T.isSuffixOf (T.pack $ "v" ++ show p) rep4 then T.append packQ (T.dropEnd (T.length $ T.pack $ "v" ++ show p) rep4) else rep4
        soln = T.unpack rep5

sillyUnwrap :: TyCon -> TyCon -> EqnSet
sillyUnwrap (PairTy a b) (PairTy c d) = [(a,c), (b,d)]
sillyUnwrap (ListTy a) (ListTy b) = [(a,b)]
sillyUnwrap (FnTy a b) (FnTy c d) = [(a,c), (b,d)]
sillyUnwrap _ _ = []

transformUnify (Nothing) = "Nothing"
transformUnify (Just x) = show $ L.sort $ H.toList x

prop_unifyRedux n = do {
                       ; let numVars = 20
                       ; rightTests <- sample' $ listOf1 (tycon' 1)
                       ; let intList = map TyVar [1..numVars]
                       ; let tests = zip intList (concat rightTests)
                       ; let actual = transformUnify $ U.unify tests
                       ; let test = sillyUnify tests []
                       ; result <- if (actual == test) then do {return True} else do { putStrLn "Failed\n\n"; print $ show tests; print actual; print test; return False}
                       ; return result
                       }

main = do {
          ; putStrLn ""
          ; putStrLn ""
          ; putStrLn "*********** QuickChecks ************"
          ; substFunTest <- testPropRet (prop_substFun U.phi, "substFun", [])
          ; monoTyLiftSubstTest <- testPropRet (prop_monoTyLiftSubst U.phi, "monoTyLiftSubst", [])
          ; occursTest <- testPropOnlyRet (prop_occurs, "occurs")
          ; if (substFunTest && monoTyLiftSubstTest && occursTest)
            then do {
                    ; let numTests = 10000
                    ; putStrLn $ "Running " ++ (show numTests) ++ " tests for unify... Please be patient!"
                    ; results <- mapM prop_unifyRedux [1..numTests]
                    ; putStrLn $ (if (and results) then "Passed" else "Failed") ++ " QuickChecks for unify."
                    }
            else do {
                    ; putStrLn "Skipping unify test."
                    }
          ; putStrLn ""
          ; putStrLn "*********** Student Tests **********"
          ; putStr "Question 1:  "
          ; print (map (\(x, y) -> (x == y)) (monoTyLiftSubstTests))
          ; putStrLn ""
          ; putStr "Question 2:  "
          ; print (map (\(x, y) -> (show x == y)) (substFunTests U.substFun))
          ; putStrLn ""
          ; putStr "Question 3:  "
          ; print (map (\(x, y) -> (x == y)) (occursTests U.occurs))
          ; putStrLn ""
          ; putStr "Question 4:  "
          ; print (map (\(x, y) -> (x == y)) (unifyTests U.unify))
          ; putStrLn ""
          ; putStr "Potpourri 1: "
          ; print (map (\(x, y) -> (x == y)) (unifyTests U.unify))
          ; putStrLn ""
          ; putStr "Potpourri 2: "
          ; print (map (\(x, y) -> (x == y)) (unifyTests U.unify))
          }
