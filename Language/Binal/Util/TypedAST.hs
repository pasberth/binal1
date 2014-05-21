module Language.Binal.Util.TypedAST where

import           Language.Binal.Types

typeof :: TypedAST -> TyKind
typeof (TyLit _ ty _) = ty
typeof (TyList _ ty _) = ty
