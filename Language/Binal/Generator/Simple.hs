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
generateStmt x = ExprStmtJSAST (generateExpr x)

generateExpr :: TypedAST -> JSAST
generateExpr (TyLit (SymLit s) _ _) = IdentJSAST s
generateExpr (TyLit (StrLit s) _ _) = StrLitJSAST s
generateExpr (TyLit (IntLit i) _ _) = NumLitJSAST (realToFrac i)
generateExpr (TyLit (NumLit i) _ _) = NumLitJSAST i
generateExpr (TyList (TyLit (SymLit "lambda") _ _:params:body:[]) _ _) = do
  let params' = generateParams params
  let body' = generateFuncBody body
  FuncLitJSAST params' body'
generateExpr x@(TyList (TyLit (SymLit "seq") _ _:_) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr x@(TyList (TyLit (SymLit "let") _ _:_) _ _) = do
  StmtExprJSAST (generateStmt x)
generateExpr x@(TyList (TyLit (SymLit "letrec") _ _:_) _ _) = do
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
generateExpr (TyList (f:args) _ _) = do
  let f' = generateExpr f
  let args' = map generateExpr args
  CallJSAST f' args'
generateExpr (TyList [] _ _) = undefined
