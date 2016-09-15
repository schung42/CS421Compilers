module Unifier where
import qualified Data.HashMap.Strict as H
import Lib

fixMe = error "fix me!"

{---------------------------------------------
 - Given Demo Environments
 -   You might not want to modify this. ;)
 ---------------------------------------------}
phi :: SubstEnv
phi = (H.insert 5 (FnTy BoolTy (TyVar 2)) H.empty)

{---------------------------------------------
 - Problem 1: substFun
 ---------------------------------------------}
substFun :: SubstEnv -> TyCon -> TyCon
substFun env (TyVar val) = H.lookupDefault (TyVar val) val env
substFun env val = val
{---------------------------------------------
 - Problem 2: monoTyLiftSubst
 ---------------------------------------------}
monoTyLiftSubst :: SubstEnv -> TyCon -> TyCon
monoTyLiftSubst env (TyVar val) = substFun env (TyVar val) 
monoTyLiftSubst env (PairTy val1 val2) = (PairTy (monoTyLiftSubst env val1) (monoTyLiftSubst env val2))
monoTyLiftSubst env (FnTy val1 val2) = (FnTy (monoTyLiftSubst env val1) (monoTyLiftSubst env val2))
monoTyLiftSubst env (ListTy val) = (ListTy (monoTyLiftSubst env val))
monoTyLiftSubst env val = val 

{---------------------------------------------
 - Problem 3: occurs
 ---------------------------------------------}
occurs :: TyCon -> TyCon -> Bool
occurs arg (TyVar target) = arg == (TyVar target)
occurs arg (PairTy target1 target2) = (occurs arg target1) || (occurs arg target2)
occurs arg (FnTy target1 target2) = (occurs arg target1) || (occurs arg target2)
occurs arg (ListTy target) = occurs arg target
occurs arg target = arg == target

{---------------------------------------------
 - Problem 4: unify
 ---------------------------------------------}
unify :: EqnSet -> Maybe SubstEnv
unify eqs = ruleApp [] eqs H.empty

-- All rules have been finished
ruleApp :: EqnSet -> EqnSet -> SubstEnv -> Maybe SubstEnv
ruleApp [] [] env = Just env

-- Finished rules, but still equations left. Start back at beginning and sub all values from env.
ruleApp head [] env = ruleApp [] (subVal head env) env

-- Rule with 2 TyVars
ruleApp head ((TyVar type1, TyVar type2):eqs) env
    | type1 == type2 = ruleApp head eqs env -- delete
    | (substFun env (TyVar type1)) /= (TyVar type1) && (substFun env (TyVar type2)) /= (TyVar type2) = ruleApp (subVal head env) (subVal eqs env) env -- both in env already. Just sub in for values.
    | (substFun env (TyVar type1)) /= (TyVar type1) = ruleApp head eqs (H.insert type2 (substFun env (TyVar type1)) env) -- type1 in env. sub it in for type2
    | (substFun env (TyVar type2)) /= (TyVar type2) = ruleApp head eqs (H.insert type1 (substFun env (TyVar type2)) env) -- type2 in env. sub it in for type1
    | otherwise                                     = ruleApp (head ++ eqs) [] (H.insert type1 (TyVar type2) env) -- add variable to env.

-- Eliminate
ruleApp head (((TyVar num, result)):eqs) env
    | (occurs (TyVar num) result) == True = ruleApp ((TyVar num, result):head) eqs env         -- has same variable on both sides. Move on.
    | otherwise                           = ruleApp (head ++ eqs) [] (H.insert num result env) -- add variable to env.

-- Orient
ruleApp head ((result, TyVar type1):eqs) env = ruleApp ((TyVar type1, result):head) eqs env

-- Decompose
ruleApp head ((PairTy one1 one2, PairTy two1 two2):eqs) env = ruleApp head ((one1, two1):(one2, two2):eqs) env
ruleApp head ((FnTy one1 one2, FnTy two1 two2):eqs) env = ruleApp head ((one1, two1):(one2, two2):eqs) env
ruleApp head ((ListTy l1, ListTy l2):eqs) env = ruleApp head ((l1, l2):eqs) env

-- Delete
ruleApp head ((x, y):eqs) env
    | x == y    = ruleApp head eqs env
    | otherwise = Nothing

-- Change all TyVar with num to TyCon with val from env
subVal [] _ = []
subVal ((x, y):xs) env = (monoTyLiftSubst env x, monoTyLiftSubst env y):(subVal xs env)


















