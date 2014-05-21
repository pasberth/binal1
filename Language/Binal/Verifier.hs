module Language.Binal.Verifier where

import           Control.Monad.State
import           Control.Lens
import qualified Data.Maybe           as Maybe
import qualified Data.HashSet        as HashSet
import qualified Data.HashMap.Strict as HashMap
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

inferTypeOfParams :: AST -> State (TypeEnv, [Variable]) TypedAST
inferTypeOfParams x@(Lit _ _) = inferType' x
inferTypeOfParams (List xs pos) = do
  xs' <- mapM inferTypeOfParams xs
  let ty = ListTy (map Util.typeof xs')
  return (TyList xs' ty pos)

inferType' :: AST -> State (TypeEnv, [Variable]) TypedAST
inferType' (Lit lit@(SymLit s) pos) = do
  env <- use _1
  let ty = Maybe.fromJust (HashMap.lookup s env)
  return (TyLit lit ty pos)
inferType' (Lit lit@(StrLit _) pos) = return (TyLit lit StrTy pos)
inferType' (Lit lit@(IntLit _) pos) = return (TyLit lit IntTy pos)
inferType' (Lit lit@(NumLit _) pos) = return (TyLit lit NumTy pos)
inferType' (List xs pos) = do
  env <- use _1
  varList <- use _2
  let instr = xs !! 0
  case instr of
    Lit (SymLit "lambda") pos1 -> do
      let params = xs !! 1
      let body = xs !! 2
      let syms = Util.flatSymbols params
      let env' = foldl (\e (sym,var) -> HashMap.insert sym (VarTy var) e) env (zip syms varList)
      _1 .= env'
      typedBody <- inferType' body
      typedParams <- inferTypeOfParams params
      _1 .= env
      return (TyList
                [TyLit (SymLit "lambda") SymTy pos1, typedParams, typedBody]
                (ArrTy (Util.typeof typedParams) (Util.typeof typedBody))
                pos)
    Lit (SymLit "seq") pos1 -> do
      xs' <- mapM inferType' (tail xs)
      return (TyList
                (TyLit (SymLit "seq") SymTy pos1:xs')
                (Util.typeof (last xs'))
                pos)
    _ -> undefined

inferType :: AST -> TypedAST
inferType ast = evalState (inferType' ast) (Util.initialTypeEnv, Util.infiniteVarList)
