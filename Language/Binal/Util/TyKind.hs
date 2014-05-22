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

extractListTy :: TyKind -> Maybe [TyKind]
extractListTy (ListTy xs) = Just xs
extractListTy _ = Nothing

flatListTy' :: [TyKind] -> [TyKind]
flatListTy' [] = []
flatListTy' xs = do
  case last xs of
    ListTy ys -> init xs ++ flatListTy' ys
    _ -> xs

flatListTy :: TyKind -> TyKind
flatListTy (VarTy i) = VarTy i
flatListTy SymTy = SymTy
flatListTy StrTy = StrTy
flatListTy IntTy = IntTy
flatListTy NumTy = NumTy
flatListTy (ArrTy ty1 ty2) = ArrTy (flatListTy ty1) (flatListTy ty2)
flatListTy (ListTy tys) = case flatListTy' tys of
  [ty] -> ty
  tys' -> ListTy tys'
