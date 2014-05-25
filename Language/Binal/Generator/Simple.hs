module Language.Binal.Generator.Simple where

import qualified Data.Aeson          as Aeson
import qualified Data.Maybe          as Maybe
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy.UTF8  as BS
import           Language.Binal.Types
import           Language.Binal.Generator.Types
import qualified Language.Binal.Util as Util

generateDeclare :: TypedAST -> JSAST
generateDeclare = DefVarsJSAST . Util.flatSymbolsT

generateParams1 :: TypedAST -> Int -> JSAST
generateParams1 (TyLit (SymLit s) _ _) _ = IdentJSAST s
generateParams1 _ i = IdentJSAST ("_" ++ show i)

generateParams :: TypedAST -> [JSAST]
generateParams (TyLit (SymLit s) _ _) = [IdentJSAST s]
generateParams (TyLit _ _ _) = []
generateParams (TyList xs _ _) = map (uncurry generateParams1) (zip xs [0..])

assignValues :: TypedAST -> JSAST -> JSAST
assignValues (TyLit (SymLit s) _ _) tmp = ExprStmtJSAST (AssignJSAST (IdentJSAST s) tmp)
assignValues (TyLit (StrLit _) _ _) _ = BlockJSAST []
assignValues (TyLit (IntLit _) _ _) _ = BlockJSAST []
assignValues (TyLit (NumLit _) _ _) _ = BlockJSAST []
assignValues (TyList xs _ _) tmp
  = BlockJSAST
      (map (\(x,i) ->
        assignValues x
          (ComputedMemberJSAST tmp (NumLitJSAST (realToFrac i)))) (zip xs ([0..] :: [Int])))

generateString :: TypedAST -> String
generateString = BS.toString . Aeson.encode . Aeson.toJSON . flatJSAST . generateProgram

generateProgram :: TypedAST -> JSAST
generateProgram tast =
  case generateStmt tast of
    BlockJSAST xs -> ProgramJSAST xs
    x -> x

generateFuncBody :: TypedAST -> JSAST
generateFuncBody tast =
  case generateStmt tast of
    BlockJSAST [] -> BlockJSAST []
    BlockJSAST xs ->
      case last xs of
        ExprStmtJSAST x -> BlockJSAST (init xs ++ [RetJSAST x])
        _ -> BlockJSAST xs
    ExprStmtJSAST x -> BlockJSAST [RetJSAST x]
    x -> BlockJSAST [x]

generateStmt :: TypedAST -> JSAST
generateStmt (TyList (TyLit (SymLit "seq") _ _:xs) _ _) = do
  BlockJSAST (map generateStmt xs)
generateStmt (TyList (TyLit (SymLit "let") _ _:pattern:value:[]) _ _) = do
  let declare = generateDeclare pattern
  let value' = generateExpr value
  case pattern of
    (TyLit (SymLit s) _ _) -> do
      let assign = ExprStmtJSAST (AssignJSAST (IdentJSAST s) value')
      BlockJSAST [declare, assign]
    _ -> do
      let tmpDeclare = DefVarsJSAST ["_tmp"]
      let tmpAssign = ExprStmtJSAST (AssignJSAST (IdentJSAST "_tmp") value')
      let assign = assignValues pattern (IdentJSAST "_tmp")
      BlockJSAST [declare, tmpDeclare, tmpAssign, assign]
generateStmt (TyList (TyLit (SymLit "letrec") _ _:pattern:value:[]) _ _) = do
  let declare = generateDeclare pattern
  let value' = generateExpr value
  case pattern of
    (TyLit (SymLit s) _ _) -> do
      let assign = ExprStmtJSAST (AssignJSAST (IdentJSAST s) value')
      BlockJSAST [declare, assign]
    _ -> do
      let tmpDeclare = DefVarsJSAST ["_tmp"]
      let tmpAssign = ExprStmtJSAST (AssignJSAST (IdentJSAST "_tmp") value')
      let assign = assignValues pattern (IdentJSAST "_tmp")
      BlockJSAST [declare, tmpDeclare, tmpAssign, assign]
generateStmt (TyList (TyLit (SymLit "assume") _ _:_:[]) _ _) = do
  BlockJSAST []
generateStmt (TyList (TyLit (SymLit "match") ty1 pos:x:xs) _ _) = do
  let value = generateExpr x
  let tmpDeclare = DefVarsJSAST ["_tmp"]
  let tmpAssign = ExprStmtJSAST (AssignJSAST (IdentJSAST "_tmp") value)
  let tmp = IdentJSAST "_tmp"
  let exprAndTypes = zip (map (\y -> generateExpr (TyList [y, TyLit (SymLit "_tmp") (Util.typeof x) (Util.whereIs x)] ty1 pos)) xs) (map Util.typeof xs)
  let fs = map (\(expr, ty) -> case ty of
                  ArrTy ty' _ -> CondJSAST (generateMatching ty' tmp) expr
                  _ -> CondJSAST (UnaryJSAST "void" (NumLitJSAST 1)) expr) exprAndTypes
  let y = ExprStmtJSAST (foldr id (UnaryJSAST "void" (NumLitJSAST 0)) fs)
  BlockJSAST [tmpDeclare, tmpAssign, y]
generateStmt x = ExprStmtJSAST (generateExpr x)

generateExpr :: TypedAST -> JSAST
generateExpr (TyLit (SymLit s) _ _) = IdentJSAST s
generateExpr (TyLit (StrLit s) _ _) = StrLitJSAST s
generateExpr (TyLit (IntLit i) _ _) = NumLitJSAST (realToFrac i)
generateExpr (TyLit (NumLit i) _ _) = NumLitJSAST i
generateExpr (TyList (TyLit (SymLit "lambda") _ _:params:body:[]) _ _) = do
  let params' = generateParams params
  let body' = generateFuncBody body
  case params' of
    [] -> FuncLitJSAST [] body'
    _ -> do
      let initParams = init params'
      let lastParam = last params'
      let sliceCall = CallJSAST (MemberJSAST (MemberJSAST (MemberJSAST (IdentJSAST "Array") "prototype") "slice") "call") [IdentJSAST "arguments", NumLitJSAST (realToFrac (length initParams))]
      let lastCheck = CondJSAST (BinaryJSAST "===" (MemberJSAST (IdentJSAST "arguments") "length") (NumLitJSAST (realToFrac (length params')))) lastParam sliceCall
      FuncLitJSAST params' (BlockJSAST [ExprStmtJSAST (AssignJSAST lastParam lastCheck), body'])
generateExpr x@(TyList (TyLit (SymLit "seq") _ _:_) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr x@(TyList (TyLit (SymLit "let") _ _:_) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr x@(TyList (TyLit (SymLit "letrec") _ _:_) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr x@(TyList (TyLit (SymLit "match") _ _:_) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr (TyList (TyLit (SymLit "object") _ _:xs) _ _) = do
  let symbols = Maybe.catMaybes (map (\(x,i) -> if even i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
  let exprs = Maybe.catMaybes (map (\(x,i) -> if odd i then Just x else Nothing) (zip (tail xs) ([0..] :: [Int])))
  let propertyNames = Maybe.catMaybes (map (\x -> case x of
                                              TyLit (SymLit s) _ _ -> Just s
                                              _ -> Nothing) symbols)
  ObjLitJSAST (HashMap.fromList (zip propertyNames (map generateExpr exprs)))
generateExpr (TyList (TyLit (SymLit ".") _ _:obj:TyLit (SymLit s) _ _:[]) _ _) = do
  let obj' = generateExpr obj
  MemberJSAST obj' s
generateExpr x@(TyList (TyLit (SymLit "assume") _ _:_:[]) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr (TyList (f:args) _ _) = do
  let f' = generateExpr f
  case args of
    [] -> CallJSAST f' []
    _ -> do
      let lastArg = last args
      let initArgs' = map generateExpr (init args)
      let lastArg' = generateExpr lastArg
      case lastArg' of
        MemberJSAST this name -> do
          let tmpThis = IdentJSAST "_tmp_this"
          let tmp = IdentJSAST "_tmp1"
          let tmpDeclare = DefVarsJSAST ["_tmp1", "_tmp_this"]
          let tmpThisAssign = ExprStmtJSAST (AssignJSAST tmpThis this)
          let tmpAssign = ExprStmtJSAST (AssignJSAST tmp (MemberJSAST tmpThis name))
          let normalCall = CallJSAST f' (initArgs' ++ [tmp])
          let alternateCall = CallJSAST (MemberJSAST f' "apply") ([tmpThis] ++ [(CallJSAST (MemberJSAST (ArrLitJSAST initArgs') "concat") [tmp])])
          let callTest = CondJSAST (BinaryJSAST "instanceof" tmp (IdentJSAST "Array"))
          let call = callTest alternateCall normalCall
          StmtExprJSAST (BlockJSAST [tmpDeclare, tmpThisAssign, tmpAssign, ExprStmtJSAST call])
        _ -> do
          let tmp = IdentJSAST "_tmp1"
          let tmpDeclare = DefVarsJSAST ["_tmp1"]
          let tmpAssign = ExprStmtJSAST (AssignJSAST tmp lastArg')
          let normalCall = CallJSAST f' (initArgs' ++ [tmp])
          let alternateCall = CallJSAST (MemberJSAST f' "apply") ([IdentJSAST "this"] ++ [(CallJSAST (MemberJSAST (ArrLitJSAST initArgs') "concat") [tmp])])
          let callTest = CondJSAST (BinaryJSAST "instanceof" tmp (IdentJSAST "Array"))
          let call = callTest alternateCall normalCall
          StmtExprJSAST (BlockJSAST [tmpDeclare, tmpAssign, ExprStmtJSAST call])
generateExpr (TyList [] _ _) = undefined

generateMatching :: TyKind -> JSAST -> JSAST
generateMatching StrTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "string")
generateMatching IntTy jast = BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "number")
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
  BinaryJSAST "&&" (BinaryJSAST "===" (UnaryJSAST "typeof" jast) (StrLitJSAST "object")) (foldr (BinaryJSAST "&&") cond1 conds)
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
