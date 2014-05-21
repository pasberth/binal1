module Language.Binal.Verifier where

import           Control.Monad
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

gensym :: TypeInferer Variable
gensym = do
  var <- uses _2 head
  _2 %= tail
  return var

makePoly :: TypedAST -> TypeInferer ()
makePoly = Util.traverseTyKindM
            (\ty -> case ty of
              VarTy i -> _4 %= HashSet.insert i
              _ -> return ())

unifyEnv :: TypeInferer ()
unifyEnv = do
  env <- use _1
  constraints <- use _3
  _1 .= HashMap.map (unify constraints) env

inferTypeOfParams :: AST -> TypeInferer TypedAST
inferTypeOfParams x@(Lit _ _) = inferType' x
inferTypeOfParams (List xs pos) = do
  xs' <- mapM inferTypeOfParams xs
  let ty = ListTy (map Util.typeof xs')
  return (TyList xs' ty pos)

inferType' :: AST -> TypeInferer TypedAST
inferType' (Lit lit@(SymLit s) pos) = do
  env <- use _1
  let ty = Maybe.fromJust (HashMap.lookup s env)
  return (TyLit lit ty pos)
inferType' (Lit lit@(StrLit _) pos) = return (TyLit lit StrTy pos)
inferType' (Lit lit@(IntLit _) pos) = return (TyLit lit IntTy pos)
inferType' (Lit lit@(NumLit _) pos) = return (TyLit lit NumTy pos)
inferType' (List xs pos) = do
  let instr = xs !! 0
  case instr of
    Lit (SymLit "lambda") pos1 -> do
      let params = xs !! 1
      let body = xs !! 2
      let syms = Util.flatSymbols params
      env <- use _1
      forM_ syms $ \sym -> do
        var <- gensym
        _1 %= HashMap.insert sym (VarTy var)
      typedBody <- inferType' body
      typedParams <- inferTypeOfParams params
      _1 .= env
      unifyEnv
      constraints <- use _3
      let unifiedBody = Util.mapTyKind (unify constraints) typedBody
      let unifiedParams = Util.mapTyKind (unify constraints) typedParams
      return (TyList
                [TyLit (SymLit "lambda") SymTy pos1, unifiedParams, unifiedBody]
                (ArrTy (Util.typeof unifiedParams) (Util.typeof unifiedBody))
                pos)
    Lit (SymLit "seq") pos1 -> do
      xs' <- mapM inferType' (tail xs)
      return (TyList
                (TyLit (SymLit "seq") SymTy pos1:xs')
                (Util.typeof (last xs'))
                pos)
    Lit (SymLit "let") pos1 -> do
      let pattern = xs !! 1
      let body = xs !! 2
      let syms = Util.flatSymbols pattern
      typedBody <- inferType' body
      makePoly typedBody
      forM_ syms $ \sym -> do
        var <- gensym
        _1 %= HashMap.insert sym (VarTy var)
      typedPattern <- inferTypeOfParams pattern
      _3 %= (Equal (Util.typeof typedBody) (Util.typeof typedPattern):)
      unifyEnv
      constraints <- use _3
      let unifiedPattern = Util.mapTyKind (unify constraints) typedPattern
      return (TyList
                [TyLit (SymLit "let") SymTy pos1, unifiedPattern, typedBody]
                (ListTy [])
                pos)
    _ -> do
      let func = head xs
      let args = tail xs
      typedFunc <- inferType' func
      typedArgs <- mapM inferType' args
      let funcTy = Util.typeof typedFunc
      let argsTy = ListTy (map Util.typeof typedArgs)
      x <- gensym
      _3 %= (Equal funcTy (ArrTy argsTy (VarTy x)):)
      unifyEnv
      constraints <- use _3
      let unifiedFunc = Util.mapTyKind (unify constraints) typedFunc
      let unifiedArgs = map (Util.mapTyKind (unify constraints)) typedArgs
      return (TyList (unifiedFunc:unifiedArgs) (VarTy x) pos)

inferType :: AST -> TypedAST
inferType ast = evalState (inferType' ast) (Util.initialTypeEnv, Util.infiniteVarList, [], HashSet.empty)

subst :: Variable -> TyKind -> TyKind -> TyKind
subst i x y@(VarTy j)
  | i == j = x
  | otherwise = y
subst _ _ SymTy = SymTy
subst _ _ StrTy = StrTy
subst _ _ IntTy = IntTy
subst _ _ NumTy = NumTy
subst i x (ArrTy y z) = ArrTy (subst i x y) (subst i x z)
subst i x (ListTy xs) = ListTy (map (subst i x) xs)

substConstraint :: Variable -> TyKind -> Constraint -> Constraint
substConstraint i y (Equal ty1 ty2) = Equal (subst i y ty1) (subst i y ty2)

unify :: [Constraint] -> TyKind -> TyKind
unify [] = id
unify (Equal s t:c)
  | s == t = unify c
  | otherwise = do
    let tmp1 = Util.extractVarTy s
    let tmp2 = Util.extractVarTy t
    let i = Maybe.fromJust tmp1
    let j = Maybe.fromJust tmp2
    if Maybe.isJust tmp1 && not (elem i (Util.freeVariables t))
      then unify (map (substConstraint i t) c) . subst i t
      else
        if Maybe.isJust tmp2 && not (elem j (Util.freeVariables s))
          then unify (map (substConstraint j s) c) . subst j s
          else
            case (s, t) of
              (ArrTy s1 s2, ArrTy t1 t2) ->
                unify (Equal s1 t1:Equal s2 t2:c)
              (ListTy xs, ListTy ys)
                | length xs == length ys ->
                  unify (map (uncurry Equal) (zip xs ys) ++ c)
                | otherwise ->
                  unify c
              _ -> id

examineAbsurds :: TypedAST -> [Absurd]
examineAbsurds (TyLit _ _ _) = []
examineAbsurds (TyList xs _ pos) = do
  let instr = xs !! 0
  case instr of
    TyLit (SymLit "lambda") _ _ -> do
      let body = xs !! 2
      examineAbsurds body
    TyLit (SymLit "seq") _ _ -> do
      concatMap examineAbsurds (tail xs)
    _ -> do
      let func = instr
      let args = tail xs
      let funcTy = Util.typeof func
      let argsTy = ListTy (map Util.typeof args)
      case funcTy of
        ArrTy srcTy _ -> do
          if srcTy == argsTy
            then examineAbsurds func ++ concatMap examineAbsurds args
            else [UnexpectedType srcTy argsTy pos]
        _ -> []
