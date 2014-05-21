module Language.Binal.Util.TyKind where

import           Language.Binal.Types

freeVariables :: TyKind -> [Variable]
freeVariables (VarTy i) = [i]
freeVariables SymTy = []
freeVariables StrTy = []
freeVariables IntTy = []
freeVariables NumTy = []
freeVariables (ArrTy x y) = freeVariables x ++ freeVariables y
freeVariables (ListTy xs) = concatMap freeVariables xs

extractVarTy :: TyKind -> Maybe Variable
extractVarTy (VarTy i) = Just i
extractVarTy _ = Nothing
