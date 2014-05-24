module Language.Binal.Util.TypedAST where

import qualified Data.Maybe as Maybe
import           Language.Binal.Types
import qualified Language.Binal.Util.LitKind as LitKind

typeof :: TypedAST -> TyKind
typeof (TyLit _ ty _) = ty
typeof (TyList _ ty _) = ty

mapTyKind :: (TyKind -> TyKind) -> TypedAST -> TypedAST
mapTyKind f (TyLit lit ty pos) = TyLit lit (f ty) pos
mapTyKind f (TyList xs ty pos) = TyList (map (mapTyKind f) xs) (f ty) pos

flatLitKindsT :: TypedAST -> [LitKind]
flatLitKindsT (TyLit lit _ _) = [lit]
flatLitKindsT (TyList xs _ _) = concatMap flatLitKindsT xs

flatSymbolsT :: TypedAST -> [String]
flatSymbolsT = Maybe.catMaybes . map LitKind.extractSym . flatLitKindsT

whereIs :: TypedAST -> Where
whereIs (TyLit _ _ pos) = pos
whereIs (TyList _ _ pos) = pos
