module Main where
import Unifier
import Parse

main = putStrLn "The unifier is what you want."

substFunParse env x = substFun env (parseTy x)
monoTyLiftSubstParse env x = monoTyLiftSubst env (parseTy x)
occursParse x y = occurs (parseTy x) (parseTy y)
unifyParse x = unify (parseListTy x)
