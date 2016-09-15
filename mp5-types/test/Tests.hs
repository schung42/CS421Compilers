module Tests where
import qualified Data.HashMap.Strict as H
import Lib
import Unifier as U

{---------------------------------------------
 - Some given environments...
 ---------------------------------------------}
emptyEnv = H.empty
env1 = foldl (\acc -> \tyvar -> H.insert tyvar (TyVar tyvar) acc) H.empty [1 .. 20]

{---------------------------------------------
 - And some (student) tests...
 ---------------------------------------------}
substFunTests f = [ (map (\x -> f H.empty (TyVar x)) [1 .. 10], "[v1,v2,v3,v4,v5,v6,v7,v8,v9,v10]")
                  , (map (\x -> f (H.insert 5 (FnTy BoolTy (TyVar 2)) H.empty) (TyVar x)) [1 .. 10], "[v1,v2,v3,v4,(Bool -> v2),v6,v7,v8,v9,v10]")
                  , (map (\x -> f (H.insert 1 BoolTy H.empty) (TyVar x)) [1 .. 10], "[Bool,v2,v3,v4,v5,v6,v7,v8,v9,v10]")
                  , (map (\x -> f (H.insert 1 BoolTy (H.insert 5 (FnTy BoolTy (TyVar 2)) H.empty)) (TyVar x)) [1 .. 10], "[Bool,v2,v3,v4,(Bool -> v2),v6,v7,v8,v9,v10]")
                  ]

showMonoTyLiftSubst unif ty = show $ monoTyLiftSubst unif ty

monoTyLiftSubstTests = [
                          (showMonoTyLiftSubst (H.insert 5 (FnTy BoolTy (TyVar 2)) H.empty) (FnTy (TyVar 1) (TyVar 5)), "(v1 -> (Bool -> v2))"),
                          (showMonoTyLiftSubst H.empty (FnTy (TyVar 1) (TyVar 2)), "(v1 -> v2)")
                       ]

occursTests f = [
                  (f (TyVar 0) (FnTy (TyVar 0) (TyVar 0)), True),
                  (f (TyVar 0) (FnTy (TyVar 1) (TyVar 2)), False)
                ]

unifyTests f = [
                  (show $ f [(TyVar 0, ListTy IntTy), ((FnTy (TyVar 0) (TyVar 0)), (FnTy (TyVar 0) (TyVar 1)))], "Just (fromList [(0,[Integer]),(1,[Integer])])")
               ]
