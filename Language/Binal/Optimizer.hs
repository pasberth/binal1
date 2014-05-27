module Language.Binal.Optimizer where

import qualified Data.Maybe          as Maybe
import           Language.Binal.Types
import qualified Language.Binal.Util as Util

isNotCall :: String -> TypedAST -> Bool
isNotCall _ (TyLit _ _ _) = True
isNotCall name (TyList (TyLit (SymLit "^") _ _:_:body:[]) _ _) = isNotCall name body
isNotCall name (TyList (TyLit (SymLit "seq") _ _:xs) _ _) = all (isNotCall name) xs
isNotCall name (TyList (TyLit (SymLit "let") _ _:_:body:[]) _ _) = isNotCall name body
isNotCall name (TyList (TyLit (SymLit "letrec") _ _:_:body:[]) _ _) = isNotCall name body
isNotCall name (TyList (TyLit (SymLit "match") _ _:xs) _ _) = all (isNotCall name) xs
isNotCall name (TyList (TyLit (SymLit "cond") _ _:xs) _ _) = all (isNotCall name) xs
isNotCall name (TyList (TyLit (SymLit ".") _ _:x:_:[]) _ _) = isNotCall name x
isNotCall name (TyList (TyLit (SymLit ":") _ _:_:x:[]) _ _) = isNotCall name x
isNotCall _ (TyList (TyLit (SymLit "assume") _ _:_) _ _) = True
isNotCall name (TyList ((TyLit (SymLit name1) _ _):args) _ _)
  | name == name1 = False
  | otherwise = all (isNotCall name) args
isNotCall name (TyList xs _ _) = all (isNotCall name) xs

isTailCall :: String -> TypedAST -> Bool
isTailCall _ (TyLit _ _ _) = True
isTailCall name (TyList (TyLit (SymLit "^") _ _:_:body:[]) _ _) = isTailCall name body
isTailCall _ (TyList (TyLit (SymLit "seq") _ _:[]) _ _) = True
isTailCall name (TyList (TyLit (SymLit "seq") _ _:args) _ _) = all (isNotCall name) (init args) && isTailCall name (last args)
isTailCall name (TyList (TyLit (SymLit "let") _ _:_:x:[]) _ _) = isNotCall name x
isTailCall name (TyList (TyLit (SymLit "letrec") _ _:_:x:[]) _ _) = isNotCall name x
isTailCall _ (TyList (TyLit (SymLit "match") _ _:[]) _ _) = True
isTailCall name (TyList (TyLit (SymLit "match") _ _:xs) _ _) = do
  isNotCall name (head xs) && all (isTailCall name) (tail xs)
isTailCall name (TyList (TyLit (SymLit "cond") _ _:xs) _ _) = do
  let ys = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (init xs) ([0..] :: [Int])))
  let zs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (init xs) ([0..] :: [Int])))
  let z = last xs

  all (isNotCall name) ys && all (isTailCall name) zs && isTailCall name z
isTailCall name (TyList (TyLit (SymLit ".") _ _:x:_:[]) _ _) = isNotCall name x
isTailCall name (TyList (TyLit (SymLit ":") _ _:_:x:[]) _ _) = isNotCall name x
isTailCall _ (TyList (TyLit (SymLit "assume") _ _:_) _ _) = True
isTailCall name (TyList xs _ _) = all (isNotCall name) xs

optimizeTailCallingToLoop :: String -> TypedAST -> TypedAST
optimizeTailCallingToLoop name (TyList (TyLit (SymLit "seq") ty1 pos1:args) ty2 pos2)
  = (TyList (TyLit (SymLit "seq") ty1 pos1: (init args ++ [optimizeTailCallingToLoop name (last args)])) ty2 pos2)
optimizeTailCallingToLoop name (TyList (TyLit (SymLit "match") ty2 pos2:xs) ty1 pos1)
  = TyList ( TyLit (SymLit "match") ty2 pos2
              : head xs
                : map (\x -> case x of
                              TyList (TyLit (SymLit "^") ty3 pos3:param:body:[]) ty4 pos4
                                 -> TyList (TyLit (SymLit "^") ty3 pos3:param:optimizeTailCallingToLoop name body:[]) ty4 pos4
                              _ -> x) (tail xs)) ty1 pos1
optimizeTailCallingToLoop name (TyList (TyLit (SymLit "cond") ty1 pos1:xs) ty2 pos2) = do
  let ys = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (init xs) ([0..] :: [Int])))
  let zs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (init xs) ([0..] :: [Int])))
  let z = optimizeTailCallingToLoop name (last xs)
  let zs1 = map (optimizeTailCallingToLoop name) zs
  TyList ( TyLit (SymLit "cond") ty1 pos1
              : concatMap (\(a,b) -> [a,b]) (zip ys zs1) ++ [z]) ty2 pos2
optimizeTailCallingToLoop name it@(TyList ((TyLit (SymLit name1) _ pos1):args) ty2 pos2)
  | name == name1 = TyList (TyLit (SymLit "recur") SymTy pos1 : args) ty2 pos2
                    {-case param of
                      TyList params _ _
                        | length params == length args
                            -> TyList (TyLit (SymLit "seq") SymTy (Util.whereIs it)
                                                : map (\(p,a) ->
                                                        TyList
                                                          [TyLit (SymLit ":=") SymTy (Util.whereIs a)
                                                          , p
                                                          , a
                                                          ]
                                                          (ListTy [])
                                                          (Util.whereIs param))
                                                          (zip params args)
                                                ++ [Sym]) (ListTy []) (Util.whereIs param)
                        | otherwise -> it
                      _ -> it-}
  | otherwise = TyList [TyLit (SymLit "terminate") SymTy pos1, it] ty2 pos2
optimizeTailCallingToLoop _ x = TyList [TyLit (SymLit "break") SymTy (Util.whereIs x), x] (Util.typeof x) (Util.whereIs x)

optimizeTailCallingLambda :: String -> TypedAST -> TypedAST
optimizeTailCallingLambda name (TyList (TyLit (SymLit "^") ty1 pos1:param:body:[]) ty2 pos2)
  = (TyList (TyLit (SymLit "^") ty1 pos1
      : param
      : TyList
          [ TyLit (SymLit "loop") SymTy (Util.whereIs body)
          , optimizeTailCallingToLoop name body
          ] (Util.typeof body) (Util.whereIs body)
      : []) ty2 pos2)
optimizeTailCallingLambda _ x = x

optimizeTailCall :: TypedAST -> TypedAST
optimizeTailCall x@(TyLit _ _ _) = x
optimizeTailCall (TyList (TyLit (SymLit "seq") ty1 pos1:xs) ty2 pos2)
  = TyList (TyLit (SymLit "seq") ty1 pos1 : map optimizeTailCall xs) ty2 pos2
optimizeTailCall x@(TyList (TyLit (SymLit "letrec") ty1 pos1:(TyLit (SymLit name) ty2 pos2):fn:[]) ty3 pos3)
  | isTailCall name fn
      = TyList (TyLit (SymLit "letrec") ty1 pos1:(TyLit (SymLit name) ty2 pos2):optimizeTailCallingLambda name fn:[]) ty3 pos3
  | otherwise = x
optimizeTailCall x = x
