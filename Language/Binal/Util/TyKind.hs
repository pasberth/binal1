module Language.Binal.Util.TyKind where

import           Control.Monad.State
import qualified Data.List as List
import qualified Data.HashMap.Strict as HashMap
import           Language.Binal.Types

freeVariables :: TyKind -> [Variable]
freeVariables (VarTy i) = [i]
freeVariables (RecTy i ty) = filter (/=i) (freeVariables ty)
freeVariables (LitTy _) = []
freeVariables SymTy = []
freeVariables StrTy = []
freeVariables NumTy = []
freeVariables BoolTy = []
freeVariables (ArrTy x y) = freeVariables x ++ freeVariables y
freeVariables (ListTy xs) = concatMap freeVariables xs
freeVariables (EitherTy xs) = concatMap freeVariables xs
freeVariables (ObjectTy _ m) = concatMap freeVariables (HashMap.elems m)
freeVariables (MutableTy ty) = freeVariables ty

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
    ListTy [] -> xs
    ListTy ys -> init xs ++ flatListTy' ys
    _ -> xs

flatListTy :: TyKind -> TyKind
flatListTy (VarTy i) = VarTy i
flatListTy (RecTy i ty) = RecTy i (flatListTy ty)
flatListTy (LitTy lit) = LitTy lit
flatListTy SymTy = SymTy
flatListTy StrTy = StrTy
flatListTy NumTy = NumTy
flatListTy BoolTy = BoolTy
flatListTy (ArrTy ty1 ty2) = ArrTy (flatListTy ty1) (flatListTy ty2)
flatListTy (ListTy tys) = case flatListTy' tys of
  [ty] -> ty
  tys' -> ListTy tys'
flatListTy (EitherTy xs) = EitherTy (map flatListTy xs)
flatListTy (ObjectTy i m) = ObjectTy i (HashMap.map flatListTy m)
flatListTy (MutableTy ty) = MutableTy (flatListTy ty)

flatEitherTy' :: Variable -> [TyKind] -> [TyKind]
flatEitherTy' _ [] = []
flatEitherTy' i xs = do
  List.nub (concatMap (\x -> case x of
                        VarTy j
                          | i == j -> []
                          | otherwise -> [VarTy j]
                        ty -> [ty]) xs)

flatEitherTy :: Variable -> TyKind -> TyKind
flatEitherTy i (EitherTy xs) = case flatEitherTy' i xs of
  [ty] -> ty
  tys -> EitherTy tys
flatEitherTy _ ty = ty

showTy' :: TyKind -> State (HashMap.HashMap Variable String, [String]) String
showTy' (VarTy i) = do
  (mp, varList) <- get
  case HashMap.lookup i mp of
    Just s -> return ('\'':s)
    Nothing -> do
      let (v, varList') = case varList of
                            [] -> (show i, [])
                            (s:ss) -> (s, ss)
      let mp' = HashMap.insert i v mp
      put (mp', varList')
      return ('\'':v)
showTy' (RecTy i ty) = do
  x <- showTy' (VarTy i)
  y <- showTy' ty
  return ("(recur " ++ x ++ " " ++ y ++ ")")
showTy' (LitTy lit) = return (show lit)
showTy' SymTy = return "symbol"
showTy' StrTy = return "string"
showTy' NumTy = return "number"
showTy' BoolTy = return "bool"
showTy' (ArrTy ty1 ty2) = do
  ty1S <- showTy' ty1
  ty2S <- showTy' ty2
  return ("(-> " ++ ty1S ++ " " ++ ty2S ++ ")")
showTy' (ListTy xs) = do
  ss <- mapM showTy' xs
  return ("(" ++ unwords ss ++ ")")
showTy' (EitherTy xs) = do
  ss <- mapM showTy' xs
  return ("(| " ++ unwords ss ++ ")")
showTy' (ObjectTy _ m) = do
  ss <- mapM (\(key, val) -> do { x <- showTy' val; return [key, x]}) (HashMap.toList m)
  return ("(obj " ++ unwords (concat ss) ++ ")")
showTy' (MutableTy ty) = do
  s <- showTy' ty
  return ("(mutable " ++ s ++ ")")

showTy :: TyKind -> String
showTy ty = evalState (showTy' ty) (HashMap.empty, map (\ch -> [ch]) ['a'..'z'])

showTy2 :: TyKind -> TyKind -> (String, String)
showTy2 ty1 ty2 = evalState (do { x <- showTy' ty1; y <- showTy' ty2; return (x, y) }) (HashMap.empty, map (\ch -> [ch]) ['a'..'z'])

traverseVarTyM :: Monad m => (TyKind -> m ()) -> TyKind -> m ()
traverseVarTyM f ty@(VarTy _) = f ty
traverseVarTyM _ (LitTy _) = return ()
traverseVarTyM _ SymTy = return ()
traverseVarTyM _ StrTy = return ()
traverseVarTyM _ NumTy = return ()
traverseVarTyM _ BoolTy = return ()
traverseVarTyM f (ArrTy ty1 ty2) = traverseVarTyM f ty1 >> traverseVarTyM f ty2
traverseVarTyM f (ListTy tys) = mapM_ (traverseVarTyM f) tys
traverseVarTyM f (EitherTy tys) = mapM_ (traverseVarTyM f) tys
traverseVarTyM f (ObjectTy _ x) = mapM_ (traverseVarTyM f) (HashMap.elems x)
traverseVarTyM f (RecTy _ ty) = traverseVarTyM f ty
traverseVarTyM f (MutableTy ty) = traverseVarTyM f ty
