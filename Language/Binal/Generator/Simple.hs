module Language.Binal.Generator.Simple(generateString) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Aeson          as Aeson
import qualified Data.Maybe          as Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy.UTF8  as BS
import           Language.Binal.Types
import           Language.Binal.Generator.Types
import qualified Language.Binal.Util as Util
import qualified Language.Binal.Generator.Util as GUtil

humanReadable :: JSAST -> JSAST
humanReadable (ExprStmtJSAST (AssignJSAST name (CondJSAST x y z))) = humanReadable (IfJSAST (humanReadable x) (humanReadable (ExprStmtJSAST (AssignJSAST name y))) (humanReadable (ExprStmtJSAST (AssignJSAST name z))))
humanReadable x@(ExprStmtJSAST (AssignJSAST _ (SeqJSAST []))) = x
humanReadable (ExprStmtJSAST (AssignJSAST x (SeqJSAST xs))) = humanReadable (BlockJSAST (map ExprStmtJSAST (init xs) ++ [ExprStmtJSAST (AssignJSAST x (last xs))]))
humanReadable (ExprStmtJSAST (AssignJSAST x y))
  | x == y = BlockJSAST []
  | otherwise = ExprStmtJSAST (AssignJSAST (humanReadable x) (humanReadable y))
humanReadable (ExprStmtJSAST (SeqJSAST ys)) = BlockJSAST (map (humanReadable . ExprStmtJSAST) ys)
humanReadable (ExprStmtJSAST (CondJSAST x y z)) = IfJSAST (humanReadable x) (humanReadable (ExprStmtJSAST y)) (humanReadable (ExprStmtJSAST z))
humanReadable (RetJSAST (SeqJSAST xs)) = BlockJSAST ((map (humanReadable . ExprStmtJSAST) (init xs)) ++ [humanReadable (RetJSAST (last xs))])
humanReadable (RetJSAST (CondJSAST x y z)) = IfJSAST (humanReadable x) (humanReadable (RetJSAST y)) (humanReadable (RetJSAST z))
humanReadable (RetJSAST (AssignJSAST name x)) = humanReadable (BlockJSAST [ExprStmtJSAST (AssignJSAST name x), RetJSAST (IdentJSAST "undefined")])
humanReadable (RetJSAST (StmtExprJSAST (ThrowJSAST x)))= humanReadable (ThrowJSAST x)
humanReadable (RetJSAST (StmtExprJSAST (RetJSAST x)))= humanReadable (RetJSAST x)
humanReadable (BlockJSAST xs) = BlockJSAST (map humanReadable xs)
humanReadable (IfJSAST x (BlockJSAST []) (BlockJSAST [])) = humanReadable (ExprStmtJSAST x)
humanReadable (IfJSAST x (BlockJSAST []) z) = humanReadable (IfJSAST (UnaryJSAST "!" x) z (BlockJSAST []))
humanReadable x@(DefVarsJSAST _) = x
humanReadable (AssignJSAST x y) = AssignJSAST (humanReadable x) (humanReadable y)
humanReadable (MemberJSAST x y) = MemberJSAST (humanReadable x) y
humanReadable (ComputedMemberJSAST x y) = ComputedMemberJSAST (humanReadable x) (humanReadable y)
humanReadable (ObjLitJSAST x) = ObjLitJSAST (HashMap.map humanReadable x)
humanReadable (ArrLitJSAST x) =  ArrLitJSAST (map humanReadable x)
humanReadable (FuncLitJSAST x y) = FuncLitJSAST (map humanReadable x) (humanReadable y)
humanReadable x@(IdentJSAST _) = x
humanReadable x@(StrLitJSAST _) = x
humanReadable x@(NumLitJSAST _) = x
humanReadable (CallJSAST x y) = CallJSAST (humanReadable x) (map humanReadable y)
humanReadable (RetJSAST x) = RetJSAST (humanReadable x)
humanReadable (ExprStmtJSAST x) = ExprStmtJSAST (humanReadable x)
humanReadable (StmtExprJSAST x) = StmtExprJSAST (humanReadable x)
humanReadable (ProgramJSAST xs) = ProgramJSAST (map humanReadable xs)
humanReadable (CondJSAST x y z) = CondJSAST (humanReadable x) (humanReadable y) (humanReadable z)
humanReadable (IfJSAST x y z) = IfJSAST (humanReadable x) (humanReadable y) (humanReadable z)
humanReadable (BinaryJSAST x y z) = BinaryJSAST x (humanReadable y) (humanReadable z)
humanReadable (UnaryJSAST x y) = UnaryJSAST x (humanReadable y)
humanReadable (SeqJSAST xs) = SeqJSAST (map humanReadable xs)
humanReadable (ThrowJSAST x) = ThrowJSAST (humanReadable x)
humanReadable (NewJSAST x y) = NewJSAST (humanReadable x) (map humanReadable y)
humanReadable (WhileJSAST x y) = WhileJSAST (humanReadable x) (humanReadable y)

generateDeclare :: TypedAST -> JSAST
generateDeclare = DefVarsJSAST . map GUtil.toJSSafeSymbol . Util.flatSymbolsT

generateParams1 :: TypedAST -> Int -> JSAST
generateParams1 (TyLit (SymLit s) _ _) _ = IdentJSAST (GUtil.toJSSafeSymbol s)
generateParams1 _ i = IdentJSAST ("_" ++ show i)

generateParams :: TypedAST -> [JSAST]
generateParams (TyLit (SymLit s) _ _) = [IdentJSAST (GUtil.toJSSafeSymbol s)]
generateParams (TyLit _ _ _) = []
generateParams (TyList xs _ _) = map (uncurry generateParams1) (zip xs [0..])

assignValues :: TypedAST -> JSAST -> JSAST
assignValues (TyLit (SymLit s) _ _) tmp = ExprStmtJSAST (AssignJSAST (IdentJSAST (GUtil.toJSSafeSymbol s)) tmp)
assignValues (TyLit (StrLit _) _ _) _ = BlockJSAST []
assignValues (TyLit (NumLit _) _ _) _ = BlockJSAST []
assignValues (TyLit (BoolLit _) _ _) _ = BlockJSAST []
assignValues (TyList xs _ _) tmp
  = BlockJSAST
      (map (\(x,i) ->
        assignValues x
          (ComputedMemberJSAST tmp (NumLitJSAST (realToFrac i)))) (zip xs ([0..] :: [Int])))

generateString :: TypedAST -> String
generateString = BS.toString . Aeson.encode . Aeson.toJSON . flatJSAST . humanReadable . flatJSAST . generateProgram

generateProgram :: TypedAST -> JSAST
generateProgram tast = do
  let x = evalState (generateStmt tast) [0..]
  ProgramJSAST [
    ExprStmtJSAST (StmtExprJSAST (
      BlockJSAST [
        ExprStmtJSAST (StrLitJSAST "use strict"),
        DefVarsJSAST ["Binal"],
        ExprStmtJSAST (AssignJSAST (IdentJSAST "Binal") (CallJSAST (IdentJSAST "require") [StrLitJSAST "binal-runtime"])),
        x]))]

generateFuncBody :: TypedAST -> JSAST
generateFuncBody tast = do
  let (result, varList) = runState (generateStmt tast) [0..]
  let declareList = map (\i -> "_tmp" ++ show i) [0..(head varList - 1)]
  let tmpDeclare = case declareList of
                    [] -> BlockJSAST []
                    _ -> DefVarsJSAST declareList
  case result of
    BlockJSAST [] -> BlockJSAST []
    BlockJSAST xs ->
      case last xs of
        ExprStmtJSAST x -> BlockJSAST (tmpDeclare : (init xs ++ [RetJSAST x]))
        _ -> BlockJSAST (tmpDeclare:xs)
    ExprStmtJSAST x -> BlockJSAST [tmpDeclare,RetJSAST x]
    x -> BlockJSAST [tmpDeclare, x]

gensym :: State [Int] Int
gensym = do
  varList <- get
  modify tail
  return (head varList)

generateStmt :: TypedAST -> State [Int] JSAST
generateStmt (TyList (TyLit (SymLit "seq") _ _:xs) _ _) = do
  BlockJSAST <$> mapM generateStmt xs
generateStmt (TyList (TyLit (SymLit "let") _ _:pattern:value:[]) _ _) = do
  let declare = generateDeclare pattern
  value' <- generateExpr value
  case pattern of
    (TyLit (SymLit s) _ _) -> do
      let assign = ExprStmtJSAST (AssignJSAST (IdentJSAST (GUtil.toJSSafeSymbol s)) value')
      return (BlockJSAST [declare, assign])
    _ -> do
      i <- gensym
      let tmp = IdentJSAST ("_tmp" ++ show i)
      let tmpAssign = ExprStmtJSAST (AssignJSAST tmp value')
      let assign = assignValues pattern tmp
      return (BlockJSAST [declare, tmpAssign, assign])
generateStmt (TyList (TyLit (SymLit "letrec") _ _:pattern:value:[]) _ _) = do
  let declare = generateDeclare pattern
  value' <- generateExpr value
  case pattern of
    (TyLit (SymLit s) _ _) -> do
      let assign = ExprStmtJSAST (AssignJSAST (IdentJSAST (GUtil.toJSSafeSymbol s)) value')
      return (BlockJSAST [declare, assign])
    _ -> do
      i <- gensym
      let tmp = IdentJSAST ("_tmp" ++ show i)
      let tmpAssign = ExprStmtJSAST (AssignJSAST tmp value')
      let assign = assignValues pattern tmp
      return (BlockJSAST [declare, tmpAssign, assign])
generateStmt (TyList (TyLit (SymLit "assume") _ _:_:[]) _ _) = do
  return (BlockJSAST [])
generateStmt x = ExprStmtJSAST <$> generateExpr x

unRet :: JSAST -> JSAST
unRet (IfJSAST x y z) = IfJSAST x (unRet y) (unRet z)
unRet (BlockJSAST xs) = BlockJSAST (map unRet xs)
unRet (RetJSAST x) = ExprStmtJSAST x
unRet x = x

generateTailRecur :: TypedAST -> TypedAST -> State [Int] JSAST
generateTailRecur params body = do
  let params' = generateParams params
  let body' = generateFuncBody body

  recurParamsS <- mapM (\_ -> (\i -> ("_tmp" ++ show i)) <$> gensym) params'
  let recurParams = map IdentJSAST recurParamsS
  let declare = DefVarsJSAST (["recur", "terminate", "_tmp_ret", "_tmp_is_recur"] ++ recurParamsS)
  let recur = FuncLitJSAST recurParams (BlockJSAST (map (\(p1,p2) -> ExprStmtJSAST (AssignJSAST p1 p2)) (zip params' recurParams)))
  let isRecur = IdentJSAST "true"
  let sliceCall = CallJSAST (MemberJSAST (MemberJSAST (MemberJSAST (IdentJSAST "Array") "prototype") "slice") "call") [IdentJSAST "arguments", NumLitJSAST 0]
  let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST (IdentJSAST "arguments") "length") (NumLitJSAST 1)) (IdentJSAST "ret") (NewJSAST (MemberJSAST (IdentJSAST "Binal") "Tuple") [sliceCall])
  let terminateBody = BlockJSAST [
                        ExprStmtJSAST (AssignJSAST (IdentJSAST "_tmp_ret") (IdentJSAST "ret"))]
  let terminate = FuncLitJSAST [IdentJSAST "ret"]
                    (BlockJSAST
                      [ ExprStmtJSAST
                          (AssignJSAST (IdentJSAST "_tmp_is_recur") (IdentJSAST "false")),
                        ExprStmtJSAST
                          (AssignJSAST (IdentJSAST "ret") lastCheck),
                        terminateBody])
  return
    (BlockJSAST
        [ declare,
          ExprStmtJSAST (AssignJSAST (IdentJSAST "recur") recur),
          ExprStmtJSAST (AssignJSAST (IdentJSAST "terminate") terminate),
          ExprStmtJSAST (AssignJSAST (IdentJSAST "_tmp_ret") (IdentJSAST "undefined")),
          ExprStmtJSAST (AssignJSAST (IdentJSAST "_tmp_is_recur") isRecur),
          WhileJSAST
            (IdentJSAST "_tmp_is_recur")
            (unRet body'),
          RetJSAST (IdentJSAST "_tmp_ret")
        ])

generateExpr :: TypedAST -> State [Int] JSAST
generateExpr (TyLit (SymLit s) _ _) = case s of
    "num.add" ->
      return (FuncLitJSAST [IdentJSAST "x", IdentJSAST "y"] (BlockJSAST [RetJSAST (BinaryJSAST "+" (IdentJSAST "x") (IdentJSAST "y"))]))
    "str.add" ->
      return (FuncLitJSAST [IdentJSAST "x", IdentJSAST "y"] (BlockJSAST [RetJSAST (BinaryJSAST "+" (IdentJSAST "x") (IdentJSAST "y"))]))
    _ -> return (IdentJSAST (GUtil.toJSSafeSymbol s))
generateExpr (TyLit (StrLit s) _ _) = return (StrLitJSAST s)
generateExpr (TyLit (NumLit i) _ _) = return (NumLitJSAST i)
generateExpr (TyLit (BoolLit True) _ _) = return (IdentJSAST "true")
generateExpr (TyLit (BoolLit False) _ _) = return (IdentJSAST "false")
generateExpr (TyList (TyLit (SymLit "^") _ _:params:body:[]) _ _) = do
  let params' = generateParams params
  body' <- case body of
                (TyList (TyLit (SymLit "loop") _ _:body1:[]) _ _)
                  -> generateTailRecur params body1
                _ -> return (generateFuncBody body)
  case params' of
    [] -> return (FuncLitJSAST [] body')
    _ -> do
      let isVarArgs (RecTy _ _) = True
          isVarArgs (VarTy _) = True
          isVarArgs (EitherTy tys) = any isVarArgs tys
          isVarArgs (ListTy []) = False
          isVarArgs (ListTy tys) = isVarArgs (last tys)
          isVarArgs _ = False

      let lastParamTyAST = case params of
                            TyList xs _ _ -> last xs
                            x -> x
      if isVarArgs (Util.typeof lastParamTyAST)
        then do
          let initParams = init params'
          let lastParam = last params'
          let sliceCall = CallJSAST (MemberJSAST (MemberJSAST (MemberJSAST (IdentJSAST "Array") "prototype") "slice") "call") [IdentJSAST "arguments", NumLitJSAST (realToFrac (length initParams))]
          let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST (IdentJSAST "arguments") "length") (NumLitJSAST (realToFrac (length params')))) lastParam (NewJSAST (MemberJSAST (IdentJSAST "Binal") "Tuple") [sliceCall])
          return (FuncLitJSAST params' (BlockJSAST [ExprStmtJSAST (AssignJSAST lastParam lastCheck), body']))
        else do
          return (FuncLitJSAST params' body')
generateExpr x@(TyList (TyLit (SymLit "seq") _ _:_) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr x@(TyList (TyLit (SymLit "let") _ _:_) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr x@(TyList (TyLit (SymLit "letrec") _ _:_) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr (TyList (TyLit (SymLit "object") _ _:xs) _ _) = do
  let symbols = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip xs ([0..] :: [Int])))
  let exprs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip xs ([0..] :: [Int])))
  let propertyNames = Maybe.catMaybes (map (\x -> case x of
                                              TyLit (SymLit s) _ _ -> Just s
                                              _ -> Nothing) symbols)
  zs <- mapM generateExpr exprs
  return (ObjLitJSAST (HashMap.fromList (zip propertyNames zs)))
generateExpr (TyList (TyLit (SymLit ".") _ _:obj:TyLit (SymLit s) _ _:[]) _ _) = do
  obj' <- generateExpr obj
  return (MemberJSAST obj' s)
generateExpr (TyList (TyLit (SymLit ":=") _ _:x:y:[]) _ _) = do
  x' <- generateExpr x
  y' <- generateExpr y
  return (AssignJSAST x' y')
generateExpr x@(TyList (TyLit (SymLit "assume") _ _:_:[]) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr (TyList (TyLit (SymLit "match") ty1 pos:x:xs) _ _) = do
  value <- generateExpr x
  (isTmpAssign, sym, tmp) <-
    case value of
      IdentJSAST s -> return (False, s, value)
      _ -> do
        i <- gensym
        let s = "_tmp" ++ show i
        return (True, s, IdentJSAST s)
  let tmpAssign = AssignJSAST tmp value
  zs <- mapM (\y -> generateExpr (TyList [y, TyLit (SymLit sym) (case Util.typeof y of
                                                                    ArrTy s _ -> s
                                                                    _ -> Util.typeof x) (Util.whereIs x)] ty1 pos)) xs
  let exprAndTypes = zip zs (map Util.typeof xs)
  let fs = map (\(expr, ty) -> case ty of
                  ArrTy ty' _ -> CondJSAST (generateMatching ty' tmp) expr
                  _ -> CondJSAST (UnaryJSAST "void" (NumLitJSAST 1)) expr) exprAndTypes
  let y = foldr id (StmtExprJSAST (ThrowJSAST (NewJSAST (MemberJSAST (IdentJSAST "Binal") "NonExhaustivePatterns") [StrLitJSAST "Non-exhaustive patterns in match"]))) fs
  if isTmpAssign
    then return (SeqJSAST [tmpAssign, y])
    else return y
generateExpr (TyList (TyLit (SymLit "cond") _ _:xs) _ _) = do
  values <- mapM generateExpr xs
  let conds = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (init values) ([0..] :: [Int])))
  let thenClauses = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip values ([0..] :: [Int])))
  let elseClause = last values
  return (foldr id elseClause (map (uncurry CondJSAST) (zip conds thenClauses)))
generateExpr (TyList (TyLit (SymLit "num.add") _ _:x:y:[]) _ _) = do
  BinaryJSAST "+" <$> generateExpr x <*> generateExpr y
generateExpr (TyList (TyLit (SymLit "str.add") _ _:x:y:[]) _ _) = do
  BinaryJSAST "+" <$> generateExpr x <*> generateExpr y
generateExpr (TyList (TyLit (SymLit "mutable") _ _:x:[]) _ _) = do
  generateExpr x
generateExpr (TyList (TyLit (SymLit "unmutable") _ _:x:[]) _ _) = do
  generateExpr x
generateExpr (TyList (f@(TyLit (SymLit "recur") _ _):args) _ _) = do
  f' <- generateExpr f
  args' <- mapM generateExpr args
  return (CallJSAST f' args')
generateExpr (TyList (f:args) _ _) = do
  let isVarArgs (RecTy _ _) = True
      isVarArgs (VarTy _) = True
      isVarArgs (EitherTy tys) = any isVarArgs tys
      isVarArgs (ListTy []) = False
      isVarArgs (ListTy tys) = isVarArgs (last tys)
      isVarArgs _ = False

  let godFunction1 this f' initArgs' lastArg' = do
        (isTmpAssignF, tmpF) <-
          case f' of
            IdentJSAST _ -> return (False, f')
            MemberJSAST _ _ -> return (False, f')
            ComputedMemberJSAST _ _ -> return (False, f')
            _ -> do
              i <- gensym
              return (True, IdentJSAST ("_tmp" ++ show i))
        (isTmpAssign, tmp) <-
          case lastArg' of
            IdentJSAST _ -> return (False ,lastArg')
            MemberJSAST _ _ -> return (False, lastArg')
            ComputedMemberJSAST _ _ -> return (False, lastArg')
            _ -> do
              i <- gensym
              return (True, IdentJSAST ("_tmp" ++ show i))
        let tmpAssignF = AssignJSAST tmpF f'
        let tmpAssign = AssignJSAST tmp lastArg'
        let normalCall = CallJSAST tmpF (initArgs' ++ [tmp])
        let noName = case initArgs' of
                      [] -> MemberJSAST tmp "xs"
                      _ -> CallJSAST (MemberJSAST (ArrLitJSAST initArgs') "concat") [MemberJSAST tmp "xs"]
        let alternateCall = CallJSAST (MemberJSAST tmpF "apply") ([this] ++ [noName])
        let callTest = CondJSAST (BinaryJSAST "instanceof" tmp (MemberJSAST (IdentJSAST "Binal") "Tuple"))
        let call = callTest alternateCall normalCall
        if isTmpAssignF
          then
            if isTmpAssign
              then return (SeqJSAST [tmpAssignF, tmpAssign, call])
              else return (SeqJSAST [tmpAssignF, call])
          else
            if isTmpAssign
              then return (SeqJSAST [tmpAssign, call])
              else return call
  let godFunction this f' initArgs' lastArg' = do
        case Util.typeof (last args) of
          ListTy [] -> do
            return (CallJSAST f' (initArgs' ++ [lastArg']))
          ListTy tys -> do
            (isTmpAssign, tmp) <-
              case lastArg' of
                IdentJSAST _ -> return (False, lastArg')
                MemberJSAST _ _ -> return (False, lastArg')
                ComputedMemberJSAST _ _ -> return (False, lastArg')
                _ -> do
                  i <- gensym
                  return (True, IdentJSAST ("_tmp" ++ show i))

            let lastAs = map (\i -> ComputedMemberJSAST (MemberJSAST tmp "xs") (NumLitJSAST (realToFrac i))) [0..(length tys - 1)]

            if isVarArgs (last tys)
              then do
                let sliceCall = CallJSAST (MemberJSAST (MemberJSAST tmp "xs") "slice") [NumLitJSAST (realToFrac (length tys - 1))]
                let lastGet = ComputedMemberJSAST (MemberJSAST tmp "xs") (NumLitJSAST (realToFrac (length tys - 1)))
                let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST (MemberJSAST tmp "xs") "length") (NumLitJSAST (realToFrac (length tys))))
                                    (CallJSAST f' ((initArgs' ++ init lastAs) ++ [lastGet]))
                                    (CallJSAST (MemberJSAST f' "apply") [this, CallJSAST (MemberJSAST (ArrLitJSAST (initArgs' ++ init lastAs)) "concat") [sliceCall]])

                if isTmpAssign
                  then do

                    let assign = ExprStmtJSAST (AssignJSAST tmp lastArg')
                    return
                      (StmtExprJSAST
                        (BlockJSAST
                          [
                            assign,
                            (ExprStmtJSAST lastCheck)
                          ]))
                  else do
                    return lastCheck
              else do
                if isTmpAssign
                  then do
                    let assign = ExprStmtJSAST (AssignJSAST tmp lastArg')
                    return
                      (StmtExprJSAST
                        (BlockJSAST
                          [
                            assign,
                            (ExprStmtJSAST (CallJSAST f' (initArgs' ++ lastAs)))
                          ]))
                  else do
                    return (CallJSAST f' (initArgs' ++ lastAs))
          _ -> do
            if isVarArgs (Util.typeof (last args))
              then godFunction1 this f' initArgs' lastArg'
              else return (CallJSAST f' (initArgs' ++ [lastArg']))


  f' <- generateExpr f
  case args of
    [] -> return (CallJSAST f' [])
    _ -> do
      let lastArg = last args
      initArgs' <- mapM generateExpr (init args)
      lastArg' <- generateExpr lastArg
      case f' of
        MemberJSAST this name -> do
          (isTmpAssignThis, tmpThis) <-
            case this of
              IdentJSAST _ -> return (False, this)
              _ -> do
                i <- gensym
                return (True, IdentJSAST ("_tmp" ++ show i))
          let tmpAssignThis = AssignJSAST tmpThis this
          if isTmpAssignThis
            then do
              x <- godFunction tmpThis (MemberJSAST tmpThis name) initArgs' lastArg'
              case x of
                SeqJSAST xs -> return (SeqJSAST (tmpAssignThis:xs))
                _ -> return (SeqJSAST [tmpAssignThis, x])
            else do
              godFunction tmpThis f' initArgs' lastArg'
        _ -> do
          godFunction (IdentJSAST "this") f' initArgs' lastArg'
generateExpr (TyList [] _ _) = undefined

generateMatching :: TyKind -> JSAST -> JSAST
generateMatching StrTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "string")
generateMatching NumTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "number")
generateMatching BoolTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "boolean")
generateMatching (ListTy []) jast = jast
generateMatching (ListTy xs) jast = do
  let conds = map (\(x, i) ->
                generateMatching x (ComputedMemberJSAST (MemberJSAST jast "xs") (NumLitJSAST (realToFrac i))))
                (zip (init xs) ([0..] :: [Int]))
  let lastGet = ComputedMemberJSAST (MemberJSAST jast "xs") (NumLitJSAST (realToFrac (length xs - 1)))
  let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST (MemberJSAST jast "xs") "length") (NumLitJSAST (realToFrac (length xs)))) lastGet (IdentJSAST "true")
  let cond1 = generateMatching (last xs) lastCheck
  BinaryJSAST "&&" (BinaryJSAST "instanceof" jast (MemberJSAST (IdentJSAST "Binal") "Tuple")) (foldr (BinaryJSAST "&&") cond1 conds)
generateMatching (VarTy _) jast = jast
generateMatching (RecTy _ _) _ = IdentJSAST "true"
generateMatching (EitherTy xs) jast = do
  let isVar (VarTy _) = True
      isVar _ = False
  if any isVar xs
    then jast
    else do
      let conds = map (\x -> generateMatching x jast) xs
      foldr1 (BinaryJSAST "||") conds
generateMatching (ObjectTy _ m) jast
  | HashMap.null m = jast
  | otherwise = do
    let matchers = map (\(key,val) -> (generateMatching val (MemberJSAST jast key))) (HashMap.toList m)
    foldr1 (BinaryJSAST "&&") matchers
generateMatching _ _ = ThrowJSAST (NewJSAST (MemberJSAST (IdentJSAST "Binal") "NonExhaustivePatterns") [StrLitJSAST "Non-exhaustive patterns"])
