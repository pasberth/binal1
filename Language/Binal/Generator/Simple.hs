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
humanReadable (RetJSAST (SeqJSAST xs)) = BlockJSAST ((map (humanReadable . ExprStmtJSAST) (init xs)) ++ [humanReadable (RetJSAST (last xs))])
humanReadable (RetJSAST (CondJSAST x y z)) = IfJSAST (humanReadable x) (humanReadable (RetJSAST y)) (humanReadable (RetJSAST z))
humanReadable (BlockJSAST xs) = BlockJSAST (map humanReadable xs)
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
assignValues (TyList xs _ _) tmp
  = BlockJSAST
      (map (\(x,i) ->
        assignValues x
          (ComputedMemberJSAST tmp (NumLitJSAST (realToFrac i)))) (zip xs ([0..] :: [Int])))

generateString :: TypedAST -> String
generateString = BS.toString . Aeson.encode . Aeson.toJSON . flatJSAST . humanReadable . flatJSAST . generateProgram

generateProgram :: TypedAST -> JSAST
generateProgram tast =
  case evalState (generateStmt tast) [0..] of
    BlockJSAST xs -> ProgramJSAST xs
    x -> x

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

generateExpr :: TypedAST -> State [Int] JSAST
generateExpr (TyLit (SymLit s) _ _) = case s of
    _ -> return (IdentJSAST (GUtil.toJSSafeSymbol s))
generateExpr (TyLit (StrLit s) _ _) = return (StrLitJSAST s)
generateExpr (TyLit (NumLit i) _ _) = return (NumLitJSAST i)
generateExpr (TyList (TyLit (SymLit "lambda") _ _:params:body:[]) _ _) = do
  let params' = generateParams params
  let body' = generateFuncBody body
  case params' of
    [] -> return (FuncLitJSAST [] body')
    _ -> do
      let initParams = init params'
      let lastParam = last params'
      let sliceCall = CallJSAST (MemberJSAST (MemberJSAST (MemberJSAST (IdentJSAST "Array") "prototype") "slice") "call") [IdentJSAST "arguments", NumLitJSAST (realToFrac (length initParams))]
      let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST (IdentJSAST "arguments") "length") (NumLitJSAST (realToFrac (length params')))) lastParam sliceCall
      return (FuncLitJSAST params' (BlockJSAST [ExprStmtJSAST (AssignJSAST lastParam lastCheck), body']))
generateExpr x@(TyList (TyLit (SymLit "seq") _ _:_) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr x@(TyList (TyLit (SymLit "let") _ _:_) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr x@(TyList (TyLit (SymLit "letrec") _ _:_) _ _) = do
  StmtExprJSAST <$> generateStmt x
generateExpr (TyList (TyLit (SymLit "object") _ _:xs) _ _) = do
  let symbols = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
  let exprs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
  let propertyNames = Maybe.catMaybes (map (\x -> case x of
                                              TyLit (SymLit s) _ _ -> Just s
                                              _ -> Nothing) symbols)
  zs <- mapM generateExpr exprs
  return (ObjLitJSAST (HashMap.fromList (zip propertyNames zs)))
generateExpr (TyList (TyLit (SymLit ".") _ _:obj:TyLit (SymLit s) _ _:[]) _ _) = do
  obj' <- generateExpr obj
  return (MemberJSAST obj' s)
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
  zs <- mapM (\y -> generateExpr (TyList [y, TyLit (SymLit sym) (Util.typeof x) (Util.whereIs x)] ty1 pos)) xs
  let exprAndTypes = zip zs (map Util.typeof xs)
  let fs = map (\(expr, ty) -> case ty of
                  ArrTy ty' _ -> CondJSAST (generateMatching ty' tmp) expr
                  _ -> CondJSAST (UnaryJSAST "void" (NumLitJSAST 1)) expr) exprAndTypes
  let y = foldr id (UnaryJSAST "void" (NumLitJSAST 0)) fs
  if isTmpAssign
    then return (SeqJSAST [tmpAssign, y])
    else return y
generateExpr (TyList (TyLit (SymLit "num.add") _ _:x:y:[]) _ _) = do
  BinaryJSAST "+" <$> generateExpr x <*> generateExpr y
generateExpr (TyList (TyLit (SymLit "str.add") _ _:x:y:[]) _ _) = do
  BinaryJSAST "+" <$> generateExpr x <*> generateExpr y
generateExpr (TyList (f:args) _ _) = do
  f' <- generateExpr f
  case args of
    [] -> return (CallJSAST f' [])
    _ -> do
      let lastArg = last args
      initArgs' <- mapM generateExpr (init args)
      lastArg' <- generateExpr lastArg
      case lastArg' of
        MemberJSAST this name -> do
          i <- gensym
          j <- gensym
          let tmpThis = IdentJSAST ("_tmp" ++ show j)
          let tmp = IdentJSAST ("_tmp" ++ show i)
          let tmpThisAssign = AssignJSAST tmpThis this
          let tmpAssign = AssignJSAST tmp (MemberJSAST tmpThis name)
          let normalCall = CallJSAST f' (initArgs' ++ [tmp])
          let noName = case initArgs' of
                        [] -> tmp
                        _ -> CallJSAST (MemberJSAST (ArrLitJSAST initArgs') "concat") [tmp]
          let alternateCall = CallJSAST (MemberJSAST f' "apply") ([tmpThis] ++ [noName])
          let callTest = CondJSAST (BinaryJSAST "instanceof" tmp (IdentJSAST "Array"))
          let call = callTest alternateCall normalCall
          return (SeqJSAST [tmpThisAssign, tmpAssign, call])
        _ -> do
          (isTmpAssignF, tmpF) <-
            case f' of
              IdentJSAST _ -> return (False, f')
              _ -> do
                i <- gensym
                return (True, IdentJSAST ("_tmp" ++ show i))
          (isTmpAssign, tmp) <-
            case lastArg' of
              IdentJSAST _ -> return (False ,lastArg')
              _ -> do
                i <- gensym
                return (True, IdentJSAST ("_tmp" ++ show i))
          let tmpAssignF = AssignJSAST tmpF f'
          let tmpAssign = AssignJSAST tmp lastArg'
          let normalCall = CallJSAST tmpF (initArgs' ++ [tmp])
          let noName = case initArgs' of
                        [] -> tmp
                        _ -> CallJSAST (MemberJSAST (ArrLitJSAST initArgs') "concat") [tmp]
          let alternateCall = CallJSAST (MemberJSAST tmpF "apply") ([IdentJSAST "this"] ++ [noName])
          let callTest = CondJSAST (BinaryJSAST "instanceof" tmp (IdentJSAST "Array"))
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
generateExpr (TyList [] _ _) = undefined

generateMatching :: TyKind -> JSAST -> JSAST
generateMatching StrTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "string")
generateMatching NumTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "number")
generateMatching (ListTy []) jast = jast
generateMatching (ListTy xs) jast = do
  let conds = map (\(x, i) ->
                generateMatching x (ComputedMemberJSAST jast (NumLitJSAST (realToFrac i))))
                (zip (init xs) ([0..] :: [Int]))
  let sliceCall = CallJSAST (MemberJSAST jast "slice") [NumLitJSAST (realToFrac (length xs - 1))]
  let lastGet = ComputedMemberJSAST jast (NumLitJSAST (realToFrac (length xs - 1)))
  let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST jast "length") (NumLitJSAST (realToFrac (length xs)))) lastGet sliceCall
  let cond1 = generateMatching (last xs) lastCheck
  BinaryJSAST "&&" (BinaryJSAST "instanceof" jast (IdentJSAST "Array")) (foldr (BinaryJSAST "&&") cond1 conds)
generateMatching (VarTy _) jast = jast
generateMatching (RecTy _ ty) jast = generateMatching ty jast
generateMatching (EitherTy xs) jast = do
  let isVar (VarTy _) = True
      isVar _ = False
  if any isVar xs
    then jast
    else do
      let conds = map (\x -> generateMatching x jast) xs
      foldr1 (BinaryJSAST "||") conds
generateMatching ty _ = UnaryJSAST "void" (StrLitJSAST (show ty))
