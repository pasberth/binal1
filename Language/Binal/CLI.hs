module Language.Binal.CLI where

import           Language.Binal.Types
import qualified Language.Binal.PrettyPrint as PP
import qualified Language.Binal.Verifier as V

checkAST :: AST -> IO (Maybe TypedAST)
checkAST ast = do
  case V.examineForms ast of
    [] -> do
      case V.examineNames ast of
        [] -> do
          let (absurds, typedAST) = V.inferType ast
          case absurds ++ V.examineAbsurds typedAST of
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
