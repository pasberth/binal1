module Language.Binal.Util.AST where

import qualified Data.Maybe as Maybe
import           Language.Binal.Types
import qualified Language.Binal.Util.LitKind as LitKind

flatLitKinds :: AST -> [LitKind]
flatLitKinds (Lit lit _) = [lit]
flatLitKinds (List xs _) = concatMap flatLitKinds xs

flatSymbols :: AST -> [String]
flatSymbols = Maybe.catMaybes . map LitKind.extractSym . flatLitKinds

whereIsAST :: AST -> Where
whereIsAST (Lit _ pos) = pos
whereIsAST (List _ pos) = pos
