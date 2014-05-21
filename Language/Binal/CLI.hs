module Language.Binal.CLI where

import           Language.Binal.Types
import qualified Language.Binal.PrettyPrint as PP
import qualified Language.Binal.Verifier as V

checkAST :: AST -> IO ()
checkAST ast = do
  case V.examineForms ast of
    [] -> do
      case V.examineNames ast of
        [] -> do
          return ()
        errs -> do
          mapM_ PP.prettyANSINotInScope errs
          return ()
    errs -> do
      mapM_ PP.prettyANSISyntaxError errs
      return ()
