module Language.Binal.CLI where

import           Language.Binal.Types
import qualified Language.Binal.Util as Util
import qualified Language.Binal.PrettyPrint as PP
import qualified Language.Binal.Verifier as V

checkAST :: AST -> IO (Maybe TypedAST)
checkAST ast = do
  case V.examineForms ast of
    [] -> do
      case V.examineNames ast of
        [] -> do
          let (absurds, typedAST) = V.inferType ast
          case absurds of
            [] -> do
              return (Just typedAST)
            errs -> do
              mapM_ PP.prettyANSIAbsurd errs
              return Nothing
        errs -> do
          mapM_ PP.prettyANSINotInScope errs
          return Nothing
    errs -> do
      mapM_ PP.prettyANSISyntaxError errs
      return Nothing

generateInterface :: TypedAST -> String
generateInterface (TyList (TyLit (SymLit "seq") _ _:xs) _ _)
  = unlines (filter (not . null) (map generateInterface xs))
generateInterface (TyList (TyLit (SymLit "let") _ _:param:_:[]) _ _)
  = do
    let prefix = "(val " ++ generateIdent param
    let len = length prefix
    prefix ++ drop len (Util.indent (len + 1) (generateType (Util.typeof param))) ++ ")"
generateInterface (TyList (TyLit (SymLit "letrec") _ _:param:_:[]) _ _)
  = do
    let prefix = "(val " ++ generateIdent param
    let len = length prefix
    prefix ++ drop len (Util.indent (len + 1) (generateType (Util.typeof param))) ++ ")"
generateInterface (TyList (TyLit (SymLit "assume") _ _:param:[]) _ _)
  = do
    let prefix = "(val " ++ generateIdent param
    let len = length prefix
    prefix ++ drop len (Util.indent (len + 1) (generateType (Util.typeof param))) ++ ")"
generateInterface _ = ""

generateIdent :: TypedAST -> String
generateIdent (TyLit name _ _) = show name
generateIdent (TyList xs _ _) = "(" ++ unwords (map generateIdent xs) ++ ")"

generateType :: TyKind -> String
generateType = Util.showTy
