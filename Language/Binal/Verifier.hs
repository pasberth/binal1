module Language.Binal.Verifier where

import           Control.Monad.State
import qualified Data.HashSet        as HashSet
import           Language.Binal.Types
import qualified Language.Binal.Util as Util

-- 特殊形式が妥当か検査する
examineForms :: AST -> [SyntaxError]
examineForms (Lit lit pos) = do
  case Util.extractSym lit of
    Just s -> do
      if HashSet.member s Util.keywords
        then [KeywordUsedAsVariable s pos]
        else []
    Nothing -> []
examineForms (List [] pos) = [UnexpectedArity 1 0 pos]
examineForms (List xs pos) = do
  let instr = xs !! 0
  case instr of
    Lit (SymLit "lambda") _ -> do
      if length xs /= 3
        then [UnexpectedArity 3 (length xs) pos]
        else do
          let params = xs !! 1
          let body = xs !! 2
          examineForms params ++ examineForms body
    Lit (SymLit "seq") _ -> do
      if length xs == 1
        then [UnexpectedArity 2 (length xs) pos]
        else concatMap examineForms (tail xs)
    Lit (SymLit "let") _ -> do
      if length xs /= 3
        then [UnexpectedArity 3 (length xs) pos]
        else do
          let pattern = xs !! 1
          let body = xs !! 2
          examineForms pattern ++ examineForms body
    _ -> concatMap examineForms xs

examineNames' :: AST -> State (HashSet.HashSet String) [NotInScope]
examineNames' (Lit (SymLit s) pos) = do
  env <- get
  if HashSet.member s env
    then return []
    else return [NotInScope s pos]
examineNames' (Lit (StrLit _) _) = return []
examineNames' (Lit (IntLit _) _) = return []
examineNames' (Lit (NumLit _) _) = return []
examineNames' (List [] _) = return []
examineNames' (List xs _) = do
  env <- get
  let instr = xs !! 0
  case instr of
    Lit (SymLit "lambda") _ -> do
      let params = xs !! 1
      let body = xs !! 2
      let env' = foldr HashSet.insert env (Util.flatSymbols params)
      put env'
      r <- examineNames' body
      put env
      return r
    Lit (SymLit "seq") _ -> do
      rs <- mapM examineNames' (tail xs)
      return (concat rs)
    Lit (SymLit "let") _ -> do
      let pattern = xs !! 1
      let body = xs !! 2
      r <- examineNames' body
      let env' = foldr HashSet.insert env (Util.flatSymbols pattern)
      put env'
      return r
    _ -> do
      rs <- mapM examineNames' xs
      return (concat rs)

examineNames :: AST -> [NotInScope]
examineNames ast = evalState (examineNames' ast) Util.primitives
