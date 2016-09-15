module Lib where
import qualified Data.HashMap.Strict as H

{-
 - Our datatype.
 - Everything is a TyCon (i.e. a Type Constructor) for simplicity.
 - The thing that sucks here is that TyVars are TyCons - because sometimes
 -   we want to specify that it's ONLY a TyVar that we want. Oh well,
 -   pattern matching to the rescue...?
 -}

data TyCon = BoolTy
           | IntTy
           | StringTy
           | PairTy TyCon TyCon
           | FnTy TyCon TyCon
           | ListTy TyCon
           | TyVar Integer
  deriving (Eq, Ord)

instance Show TyCon where
  show BoolTy = "Bool"
  show IntTy = "Integer"
  show StringTy = "String"
  show (PairTy a b) = "(" ++ (show a) ++ ", " ++ (show b) ++ ")"
  show (FnTy a b) = "(" ++ (show a) ++ " -> " ++ (show b) ++ ")"
  show (ListTy a) = "[" ++ (show a) ++ "]"
  show (TyVar s) = "v" ++ (show s)

-- SubstEnv should really only take just TyVars as the first param and another TyCon for the second.
type SubstEnv = (H.HashMap Integer TyCon)
type EqnSet = [(TyCon, TyCon)]

