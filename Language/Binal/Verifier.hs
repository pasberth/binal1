module Language.Binal.Verifier where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Control.Lens
import qualified Data.Maybe          as Maybe
import qualified Data.List           as List
import qualified Data.HashSet        as HashSet
import qualified Data.HashMap.Strict as HashMap
import           Language.Binal.Types
import qualified Language.Binal.Util as Util

examineFormOfParams :: AST -> [SyntaxError]
examineFormOfParams lit@(Lit _ _) = examineForms lit
examineFormOfParams (List [] _) = []
examineFormOfParams (List [_] pos) = [UnexpectedArity 2 1 pos]
examineFormOfParams (List xs _) = concatMap examineFormOfParams xs

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
    Lit (SymLit "^") _ -> do
      if length xs /= 3
        then [UnexpectedArity 3 (length xs) pos]
        else do
          let params = xs !! 1
          let body = xs !! 2
          examineFormOfParams params ++ examineForms body
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
          examineFormOfParams pattern ++ examineForms body
    Lit (SymLit "letrec") _ -> do
      if length xs /= 3
        then [UnexpectedArity 3 (length xs) pos]
        else do
          let pattern = xs !! 1
          let body = xs !! 2
          examineFormOfParams pattern ++ examineForms body
    Lit (SymLit "match") _ -> do
      if length xs < 3
        then [UnexpectedArity 3 (length xs) pos]
        else concatMap examineForms (tail xs)
    Lit (SymLit "cond") _ -> do
      if odd (length xs)
        then [UnexpectedArity (length xs + 1) (length xs) pos]
        else concatMap examineForms (tail xs)
    Lit (SymLit "object") _ -> do
      if odd (length (tail xs))
        then [UnexpectedArity (length (tail xs) + 1) (length (tail xs)) pos]
        else do
          let symbols = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
          let exprs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
          let r1 = concatMap
                    (\x -> case x of
                      Lit (SymLit _) _ -> []
                      _ -> [Malformed (Util.whereIsAST x)]) symbols
          r1 ++ concatMap examineForms exprs
    Lit (SymLit ".") _ -> do
      if length xs /= 3
        then [UnexpectedArity 3 (length xs) pos]
        else
          case xs !! 2 of
            Lit (SymLit _) _ -> examineForms (xs !! 1)
            _ -> Malformed (Util.whereIsAST (xs !! 2)) : examineForms (xs !! 1)
    Lit (SymLit ":=") _ -> do
      let ex1 it@(Lit (SymLit _) _) = examineForms it
          ex1 (Lit _ pos1) = [Malformed pos1]
          ex1 it@(List [] _) = examineForms it
          ex1 (List ys pos1) = do
            let instr1 = ys !! 0
            case instr1 of
              Lit (SymLit ".") _ -> do
                if length ys /= 3
                  then [UnexpectedArity 3 (length ys) pos1]
                  else
                    case ys !! 2 of
                      Lit (SymLit _) _ -> ex1 (ys !! 1)
                      _ -> Malformed (Util.whereIsAST (ys !! 2)) : ex1 (ys !! 1)
              _ -> [Malformed pos1]
      if length xs /= 3
        then [UnexpectedArity 3 (length xs) pos]
        else
          ex1 (xs !! 1) ++ examineForms (xs !! 2)
    Lit (SymLit "assume") _ -> do
      if length xs /= 2
        then [UnexpectedArity 2 (length xs) pos]
        else
          case xs !! 1 of
            Lit (SymLit _) _ -> []
            _ -> [Malformed (Util.whereIsAST (xs !! 1))]
    _ -> concatMap examineForms xs

examineNames' :: AST -> State (HashSet.HashSet String) [NotInScope]
examineNames' (Lit (SymLit s) pos) = do
  env <- get
  if HashSet.member s env
    then return []
    else return [NotInScope s pos]
examineNames' (Lit (StrLit _) _) = return []
examineNames' (Lit (NumLit _) _) = return []
examineNames' (Lit (BoolLit _) _) = return []
examineNames' (List [] _) = return []
examineNames' (List xs _) = do
  env <- get
  let instr = xs !! 0
  case instr of
    Lit (SymLit "^") _ -> do
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
    Lit (SymLit "letrec") _ -> do
      let pattern = xs !! 1
      let body = xs !! 2
      let env' = foldr HashSet.insert env (Util.flatSymbols pattern)
      put env'
      examineNames' body
    Lit (SymLit "match") _ -> do
      rs <- mapM examineNames' (tail xs)
      return (concat rs)
    Lit (SymLit "cond") _ -> do
      rs <- mapM examineNames' (tail xs)
      return (concat rs)
    Lit (SymLit "object") _ -> do
      let exprs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
      rs <- mapM examineNames' exprs
      return (concat rs)
    Lit (SymLit ".") _ -> do
      examineNames' (xs !! 1)
    Lit (SymLit ":=") _ -> do
      rs <- mapM examineNames' (tail xs)
      return (concat rs)
    Lit (SymLit "assume") _ -> do
      let Lit (SymLit s) _ = xs !! 1
      let env' = HashSet.insert s env
      put env'
      return []
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
makePoly (TyLit _ ty1 _)
  = Util.traverseVarTyM
      (\ty -> case ty of
        VarTy i -> _4 %= HashSet.insert i
        _ -> return ())
      ty1
makePoly (TyList (TyLit (SymLit "^") _ _:_) ty1 _)
  = Util.traverseVarTyM
      (\ty -> case ty of
        VarTy i -> _4 %= HashSet.insert i
        _ -> return ())
      ty1
makePoly _ = return ()

freshPoly' :: TyKind -> StateT (HashMap.HashMap Variable Variable) (State (TypeEnv, [Variable], [Constraint], PolyEnv)) TyKind
freshPoly' (VarTy i) = do
  isPoly <- lift (uses _4 (HashSet.member i))
  if isPoly
    then do
      polys <- get
      case HashMap.lookup i polys of
        Just poly -> return (VarTy poly)
        Nothing -> do
          var <- lift gensym
          modify (HashMap.insert i var)
          return (VarTy var)
    else return (VarTy i)
freshPoly' (RecTy i ty) = do
  VarTy i' <- freshPoly' (VarTy i)
  ty' <- freshPoly' ty
  return (RecTy i' ty')
freshPoly' SymTy = return SymTy
freshPoly' StrTy = return StrTy
freshPoly' NumTy = return NumTy
freshPoly' BoolTy = return BoolTy
freshPoly' (ArrTy x y) = ArrTy <$> freshPoly' x <*> freshPoly' y
freshPoly' (ListTy tys) = ListTy <$> mapM freshPoly' tys
freshPoly' (EitherTy tys) = EitherTy <$> mapM freshPoly' tys
freshPoly' (ObjectTy i xs) = do
  xs' <- mapM (\(key, val) -> do { x <- freshPoly' val ; return (key, x) }) (HashMap.toList xs)
  return (ObjectTy i (HashMap.fromList xs'))
freshPoly' (MutableTy ty) = MutableTy <$> freshPoly' ty

freshPoly :: TyKind -> TypeInferer TyKind
freshPoly ty = evalStateT (freshPoly' ty) HashMap.empty

unifyEnv :: TypeInferer ()
unifyEnv = do
  env <- use _1
  constraints <- use _3
  _1 .= HashMap.map (unify (reverse constraints)) env

inferTypeOfParams :: AST -> TypeInferer TypedAST
inferTypeOfParams x@(Lit _ _) = inferType' x
inferTypeOfParams (List xs pos) = do
  xs' <- mapM inferTypeOfParams xs
  let ty = ListTy (map Util.typeof xs')
  return (TyList xs' ty pos)

inferType' :: AST -> TypeInferer TypedAST
inferType' (Lit lit@(SymLit "true") pos) = return (TyLit lit BoolTy pos)
inferType' (Lit lit@(SymLit "false") pos) = return (TyLit lit BoolTy pos)
inferType' (Lit lit@(SymLit s) pos) = do
  env <- use _1
  ty <- freshPoly (Maybe.fromJust (HashMap.lookup s env))
  return (TyLit lit ty pos)
inferType' (Lit lit@(StrLit _) pos) = return (TyLit lit StrTy pos)
inferType' (Lit lit@(NumLit _) pos) = return (TyLit lit NumTy pos)
inferType' (Lit lit@(BoolLit _) pos) = return (TyLit lit BoolTy pos)
inferType' (List xs pos) = do
  let instr = xs !! 0
  case instr of
    Lit (SymLit "^") pos1 -> do
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
      let unifiedBody = Util.mapTyKind (unify (reverse constraints)) typedBody
      let unifiedParams = Util.mapTyKind (unify (reverse constraints)) typedParams
      return (TyList
                [TyLit (SymLit "^") SymTy pos1, unifiedParams, unifiedBody]
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
      let bodyTy = Util.typeof typedBody
      let patTy = Util.typeof typedPattern
      let absurd = UnexpectedType bodyTy patTy (Util.whereIs typedPattern)
      _3 %= (Subtype bodyTy patTy absurd :)
      unifyEnv
      constraints <- use _3
      let unifiedPattern = Util.mapTyKind (unify (reverse constraints)) typedPattern
      return (TyList
                [TyLit (SymLit "let") SymTy pos1, unifiedPattern, typedBody]
                (ListTy [])
                pos)
    Lit (SymLit "letrec") pos1 -> do
      let pattern = xs !! 1
      let body = xs !! 2
      let syms = Util.flatSymbols pattern
      forM_ syms $ \sym -> do
        var <- gensym
        _1 %= HashMap.insert sym (VarTy var)
      typedBody <- inferType' body
      typedPattern <- inferTypeOfParams pattern
      let bodyTy = Util.typeof typedBody
      let patTy = Util.typeof typedPattern
      let absurd = UnexpectedType bodyTy patTy (Util.whereIs typedPattern)
      _3 %= (Subtype bodyTy patTy absurd :)
      unifyEnv
      constraints <- use _3
      let unifiedBody = Util.mapTyKind (unify (reverse constraints)) typedBody
      let unifiedPattern = Util.mapTyKind (unify (reverse constraints)) typedPattern
      makePoly unifiedBody
      return (TyList
                [TyLit (SymLit "letrec") SymTy pos1, unifiedPattern, unifiedBody]
                (ListTy [])
                pos)
    Lit (SymLit "match") pos1 -> do
      expr <- inferType' (xs !! 1)
      patterns <- mapM inferType' (drop 2 xs)
      syms <- mapM (\_ -> (,) <$> gensym <*> gensym) patterns
      forM_ (zip patterns syms) $ \(pat, (x, y)) -> do
        let patTy = Util.typeof pat
        let expected = ArrTy (VarTy x) (VarTy y)
        _3 %= (Subtype patTy expected (UnexpectedType expected patTy (Util.whereIs pat)) :)
      let exprTy = Util.typeof expr
      let srcTys = map (\(x, _) -> VarTy x) syms
      let expected = EitherTy srcTys
      let retTy = EitherTy (map (\(_, y) -> VarTy y) syms)
      _3 %= (Subtype exprTy expected (UnexpectedType expected exprTy (Util.whereIs expr)) :)
      unifyEnv
      constraints <- use _3
      env <- use _1
      _1 .= HashMap.map (Util.flatEitherTy (negate 1)) env
      let unifiedExpr = Util.mapTyKind (Util.flatEitherTy (negate 1) . unify (reverse constraints)) expr
      let unifiedPatterns = map (Util.mapTyKind (Util.flatEitherTy (negate 1) . unify (reverse constraints))) patterns
      return (TyList
              (TyLit (SymLit "match") SymTy pos1:unifiedExpr:unifiedPatterns)
              (Util.flatEitherTy (negate 1) (unify (reverse constraints) retTy))
              pos)
    Lit (SymLit "cond") pos1 -> do
      exprs <- mapM inferType' (tail xs)
      let conds = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (init exprs) ([0..] :: [Int])))
      let thenClauses = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip exprs ([0..] :: [Int])))
      let elseClause = last exprs
      forM_ conds $ \cond -> do
        let ty = Util.typeof cond
        _3 %= (Subtype ty BoolTy (UnexpectedType BoolTy ty (Util.whereIs cond)) :)
      unifyEnv
      constraints <- use _3
      let retTy = Util.flatEitherTy (negate 1) (EitherTy (map Util.typeof thenClauses ++ [Util.typeof elseClause]))
      return (Util.mapTyKind
                (unify (reverse constraints))
                (TyList
                  (TyLit (SymLit "cond") SymTy pos1:exprs)
                  retTy
                  pos))
    Lit (SymLit "object") pos1 -> do
      let symbols = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
      let exprs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
      let propertyNames = Maybe.catMaybes (map (\x -> case x of
                                                  Lit (SymLit s) _ -> Just s
                                                  _ -> Nothing) symbols)
      typedExprs <- mapM inferType' exprs
      let tys = map Util.typeof typedExprs
      i <- gensym
      let ty = ObjectTy (HashSet.singleton i) (HashMap.fromList (zip propertyNames tys))
      return
        (TyList
          (TyLit (SymLit "object") SymTy pos1:concatMap (\(k1, (k,v)) -> [TyLit (SymLit k) SymTy (Util.whereIsAST k1),v]) (zip symbols (zip propertyNames typedExprs)))
          ty
          pos)
    Lit (SymLit ".") pos1 -> do
      let Lit (SymLit propertyName) pos2 = xs !! 2
      let expr = xs !! 1
      typedExpr <- inferType' expr
      let exprTy = Util.typeof typedExpr
      i <- gensym
      x <- gensym
      let typedProp = TyLit (SymLit propertyName) (VarTy x) pos2
      let expected = ObjectTy (HashSet.singleton i) (HashMap.singleton propertyName (VarTy x))
      _3 %= (Equal expected exprTy (UnexpectedType expected exprTy (Util.whereIs typedExpr)) :)
      unifyEnv
      constraints <- use _3
      let unifiedExpr = Util.mapTyKind (unify (reverse constraints)) typedExpr
      let unifiedProp = Util.mapTyKind (unify (reverse constraints)) typedProp
      return
        (Util.mapTyKind
          (unify (reverse constraints))
          (TyList
            [TyLit (SymLit ".") SymTy pos1, unifiedExpr, unifiedProp]
            (VarTy x)
            pos))
    Lit (SymLit ":=") pos1 -> do
      left <- inferType' (xs !! 1)
      right <- inferType' (xs !! 2)
      let leftTy = Util.typeof left
      let expected = MutableTy (Util.typeof right)
      _3 %= (Subtype expected leftTy (UnexpectedType expected leftTy (Util.whereIs left)) :)
      unifyEnv
      constraints <- use _3
      return
        (Util.mapTyKind
          (unify (reverse constraints))
          (TyList
                [TyLit (SymLit ":=") SymTy pos1, left, right]
                (ListTy [])
                pos))
    Lit (SymLit "assume") pos1 -> do
      let Lit (SymLit sym) pos2 = xs !! 1
      var <- gensym
      _1 %= HashMap.insert sym (VarTy var)
      return (TyList
                [ TyLit (SymLit "assume") SymTy pos1,
                  TyLit (SymLit sym) (VarTy var) pos2
                ] (ListTy []) pos)
    _ -> do
      let func = head xs
      let args = tail xs
      typedFunc <- inferType' func
      typedArgs <- mapM inferType' args
      let funcTy = Util.typeof typedFunc
      let argsTy = Util.flatListTy (ListTy (map Util.typeof typedArgs))
      x <- gensym
      let expected = ArrTy argsTy (VarTy x)
      _3 %= (Subtype funcTy expected (UnexpectedType expected funcTy (Util.whereIs typedFunc)) :)
      unifyEnv
      constraints <- use _3
      let unifiedFunc = Util.mapTyKind (unify (reverse constraints)) typedFunc
      let unifiedArgs = map (Util.mapTyKind (unify (reverse constraints))) typedArgs
      return (Util.mapTyKind (unify (reverse constraints)) (TyList (unifiedFunc:unifiedArgs) (VarTy x) pos))

inferType :: AST -> ([Absurd], TypedAST)
inferType ast = do
  let (typedAST, (_, _, constraints, _)) = runState (inferType' ast) (Util.initialTypeEnv, Util.initialVarList, [], Util.initialPolyEnv)
  let absurds = cantUnify (reverse constraints)
  (List.nub absurds, Util.mapTyKind (unify (reverse constraints) . Util.flatListTy) typedAST)

subst :: Variable -> TyKind -> TyKind -> TyKind
subst i x y@(VarTy j)
  | i == j = x
  | otherwise = y
subst i x (RecTy j ty)
  | i == j = RecTy j ty
  | otherwise = RecTy j (subst i x ty)
subst _ _ SymTy = SymTy
subst _ _ StrTy = StrTy
subst _ _ NumTy = NumTy
subst _ _ BoolTy = BoolTy
subst i x (ArrTy y z) = ArrTy (subst i x y) (subst i x z)
subst i x (ListTy xs) = ListTy (map (subst i x) xs)
subst i x (EitherTy xs) = EitherTy (map (subst i x) xs)
subst i x@(ObjectTy j ys) (ObjectTy k xs)
  | HashSet.member i k = ObjectTy (HashSet.union j k) (HashMap.union xs ys)
  | otherwise = ObjectTy k (HashMap.map (subst i x) xs)
subst i x (ObjectTy j xs) = ObjectTy j (HashMap.map (subst i x) xs)
subst i x (MutableTy ty) = MutableTy (subst i x ty)

substConstraint :: Variable -> TyKind -> Constraint -> Constraint
substConstraint i y (Equal ty1 ty2 absurd) = Equal (subst i y ty1) (subst i y ty2) (substAbsurd i y absurd)
substConstraint i y (Subtype ty1 ty2 absurd) = Subtype (subst i y ty1) (subst i y ty2) (substAbsurd i y absurd)

substAbsurd :: Variable -> TyKind -> Absurd -> Absurd
substAbsurd i y (UnexpectedType ty1 ty2 pos) = UnexpectedType (subst i y ty1) (subst i y ty2) pos

unify :: [Constraint] -> TyKind -> TyKind
unify = snd . unify'

cantUnify :: [Constraint] -> [Absurd]
cantUnify = fst . unify'

unify' :: [Constraint] -> ([Absurd], TyKind -> TyKind)
unify' [] = ([], id)
unify' (Subtype s t absurd:c)
  | s == t = unify' c
  | otherwise = do
    let tmp1 = Util.extractVarTy s
    let tmp2 = Util.extractVarTy t
    let i = Maybe.fromJust tmp1
    let j = Maybe.fromJust tmp2
    if Maybe.isJust tmp1
      then do
        let t' = Util.flatEitherTy i t
        if not (elem i (Util.freeVariables t'))
          then do
            let (absurds, substitution) = unify' (map (substConstraint i t') c)
            (absurds, substitution . subst i t')
          else do
            let (absurds, substitution) = unify' (map (substConstraint i (RecTy i t')) c)
            (absurds, substitution . subst i (RecTy i t'))
      else
        if Maybe.isJust tmp2
          then do
            let s' = Util.flatEitherTy j s
            if not (elem j (Util.freeVariables s'))
              then do
                let (absurds, substitution) = unify' (map (substConstraint j s') c)
                (absurds, substitution . subst j s')
              else do
                let (absurds, substitution) = unify' (map (substConstraint j (RecTy j s')) c)
                (absurds, substitution . subst j (RecTy j s'))
          else
            case (s, t) of
              (ArrTy s1 s2, ArrTy t1 t2) ->
                unify' (Subtype t1 s1 absurd:Subtype s2 t2 absurd:c)
              (RecTy _ s1, RecTy _ t1) -> do
                unify' (Subtype s1 t1 absurd:c)
              (MutableTy s1, MutableTy t1) ->
                unify' (Subtype s1 t1 absurd:c)
              (EitherTy ss, _) -> do
                let results = map (\s1 -> unify' [Subtype s1 t absurd]) ss
                if all (\(absurds, _) -> null absurds) results
                  then do
                    let substitution = foldr (\(_, substitution1) substitution2 -> substitution1 . substitution2) id results
                    let (absurds, substitution1) = unify' c
                    (absurds, substitution1 . substitution)
                  else do
                    let (absurds, substitution) = unify' c
                    let (absurds1, substitution1) = head results
                    (absurds ++ absurds1, substitution . substitution1)
              (_, EitherTy ts) -> do
                let results = map (\t1 -> unify' (Subtype s t1 absurd:c)) ts
                let r = foldl1 (\(absurds1, substitution1) (absurds2, substitution2) ->
                                case absurds1 of
                                  [] -> (absurds1, substitution1)
                                  _ -> (absurds2, substitution2)) results
                case fst r of
                  [] -> r
                  _ -> head results
              (RecTy k s1, _) ->
                unify' (Subtype (subst k s s1) t absurd:c)
              (_, RecTy k t1) ->
                unify' (Subtype s (subst k t t1) absurd:c)
              (ListTy [], _) -> do
                let (absurds, substitution) = unify' c
                (absurd:absurds, substitution)
              (_, ListTy []) -> do
                let (absurds, substitution) = unify' c
                (absurd:absurds, substitution)
              (ListTy xs, ListTy ys)
                | length xs == length ys ->
                  unify' (map (\(a,b) -> Subtype a b absurd) (zip xs ys) ++ c)
                | length xs < length ys -> do
                  let len = length xs - 1
                  let xs1 = take len xs
                  let ys1 = take len ys
                  let xs2 = last xs
                  let ys2 = ListTy (drop len ys)
                  unify' (map (\(a,b) -> Subtype a b absurd) (zip xs1 ys1)
                            ++ [Subtype xs2 ys2 absurd]
                            ++ c)
                | length xs > length ys -> do
                  let len = length ys - 1
                  let xs1 = take len xs
                  let ys1 = take len ys
                  let xs2 = ListTy (drop len xs)
                  let ys2 = last ys
                  unify' (map (\(a,b) -> Subtype a b absurd) (zip xs1 ys1)
                            ++ [Subtype xs2 ys2 absurd]
                            ++ c)
              (ObjectTy _ xs, ObjectTy _ ys) -> do
                let xKeys = HashMap.keys xs
                let yKeys = HashMap.keys ys
                if all (\x -> elem x xKeys) yKeys
                  then do
                    let c' = map (\key -> Subtype
                                    (Maybe.fromJust (HashMap.lookup key xs))
                                    (Maybe.fromJust (HashMap.lookup key ys))
                                    absurd) yKeys
                    unify' (c'++c)
                  else do
                    let (absurds, substitution) = unify' c
                    (absurd:absurds, substitution)
              _ -> do
                unify' (Equal s t absurd:c)
unify' (Equal s t absurd:c)
  | s == t = unify' c
  | otherwise = do
    let tmp1 = Util.extractVarTy s
    let tmp2 = Util.extractVarTy t
    let i = Maybe.fromJust tmp1
    let j = Maybe.fromJust tmp2
    if Maybe.isJust tmp1
      then do
        let t' = Util.flatEitherTy i t
        if not (elem i (Util.freeVariables t'))
          then do
            let (absurds, substitution) = unify' (map (substConstraint i t') c)
            (absurds, substitution . subst i t')
          else do
            let (absurds, substitution) = unify' (map (substConstraint i (RecTy i t')) c)
            (absurds, substitution . subst i (RecTy i t'))
      else
        if Maybe.isJust tmp2
          then do
            let s' = Util.flatEitherTy j s
            if not (elem j (Util.freeVariables s'))
              then do
                let (absurds, substitution) = unify' (map (substConstraint j s') c)
                (absurds, substitution . subst j s')
              else do
                let (absurds, substitution) = unify' (map (substConstraint j (RecTy j s')) c)
                (absurds, substitution . subst j (RecTy j s'))
          else
            case (s, t) of
              (ArrTy s1 s2, ArrTy t1 t2) ->
                unify' (Equal t1 s1 absurd:Equal s2 t2 absurd:c)
              (ListTy [], _) -> do
                let (absurds, substitution) = unify' c
                (absurd:absurds, substitution)
              (_, ListTy []) -> do
                let (absurds, substitution) = unify' c
                (absurd:absurds, substitution)
              (ListTy xs, ListTy ys)
                | length xs == length ys ->
                  unify' (map (\(a,b) -> Equal a b absurd) (zip xs ys) ++ c)
                | length xs < length ys -> do
                  let len = length xs - 1
                  let xs1 = take len xs
                  let ys1 = take len ys
                  let xs2 = last xs
                  let ys2 = ListTy (drop len ys)
                  unify' (map (\(a,b) -> Equal a b absurd) (zip xs1 ys1)
                          ++ [Equal xs2 ys2 absurd]
                          ++ c)
                | length xs > length ys -> do
                  let len = length ys - 1
                  let xs1 = take len xs
                  let ys1 = take len ys
                  let xs2 = ListTy (drop len xs)
                  let ys2 = last ys
                  unify' (map (\(a,b) -> Equal a b absurd) (zip xs1 ys1)
                          ++ [Equal xs2 ys2 absurd]
                          ++ c)
              (ObjectTy k xs, ObjectTy l ys) -> do
                let xKeys = HashMap.keys xs
                let yKeys = HashMap.keys ys
                if any (\x -> elem x yKeys) xKeys
                  then do
                    unify' (Subtype s t absurd:c)
                  else do
                    let (absurds, substitution) = unify' c
                    (absurds, substitution . foldl (\f v -> subst v t . f) id (HashSet.toList k) . foldl (\f v -> subst v s . f) id (HashSet.toList l))
              (RecTy _ s1, RecTy _ t1) -> do
                unify' (Equal s1 t1 absurd:c)
              _ -> do
                let (absurds, substitution) = unify' c
                (absurd:absurds, substitution)
