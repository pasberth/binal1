module Language.Binal.Util.TypedAST where

import           Language.Binal.Types

typeof :: TypedAST -> TyKind
typeof (TyLit _ ty _) = ty
typeof (TyList _ ty _) = ty

mapTyKind :: (TyKind -> TyKind) -> TypedAST -> TypedAST
mapTyKind f (TyLit lit ty pos) = TyLit lit (f ty) pos
mapTyKind f (TyList xs ty pos) = TyList (map (mapTyKind f) xs) (f ty) pos

traverseTyKindM :: Monad m => (TyKind -> m ()) -> TypedAST -> m ()
traverseTyKindM f (TyLit _ ty _) = f ty
traverseTyKindM f (TyList xs ty _) = f ty >> mapM_ (traverseTyKindM f) xs

whereIs :: TypedAST -> Where
whereIs (TyLit _ _ pos) = pos
whereIs (TyList _ _ pos) = pos
