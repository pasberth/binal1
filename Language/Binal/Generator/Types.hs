module Language.Binal.Generator.Types where

import qualified Data.Text as Text
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap

data JSAST
  = DefVarsJSAST [String]
  | AssignJSAST JSAST JSAST
  | MemberJSAST JSAST String
  | ComputedMemberJSAST JSAST JSAST
  | ObjLitJSAST (HashMap.HashMap String JSAST)
  | FuncLitJSAST [JSAST] JSAST
  | IdentJSAST String
  | StrLitJSAST String
  | NumLitJSAST Double
  | BlockJSAST [JSAST]
  | CallJSAST JSAST [JSAST]
  | RetJSAST JSAST
  | ExprStmtJSAST JSAST
  | StmtExprJSAST JSAST
  | ProgramJSAST [JSAST]
  | CondJSAST JSAST JSAST JSAST
  | UnaryJSAST String JSAST
  | BinaryJSAST String JSAST JSAST

flatJSAST' :: JSAST -> [JSAST]
flatJSAST' (BlockJSAST xs) = concatMap flatJSAST' xs
flatJSAST' (DefVarsJSAST x) = [DefVarsJSAST x]
flatJSAST' (AssignJSAST x y) = [AssignJSAST (flatJSAST x) (flatJSAST y)]
flatJSAST' (MemberJSAST x y) = [MemberJSAST (flatJSAST x) y]
flatJSAST' (ComputedMemberJSAST x y) = [ComputedMemberJSAST (flatJSAST x) (flatJSAST y)]
flatJSAST' (ObjLitJSAST x) =  [ObjLitJSAST (HashMap.map flatJSAST x)]
flatJSAST' (FuncLitJSAST x y) = [FuncLitJSAST (map flatJSAST x) (flatJSAST y)]
flatJSAST' (IdentJSAST s) = [IdentJSAST s]
flatJSAST' (StrLitJSAST s) = [StrLitJSAST s]
flatJSAST' (NumLitJSAST n) = [NumLitJSAST n]
flatJSAST' (CallJSAST x y) = [CallJSAST (flatJSAST x) (map flatJSAST y)]
flatJSAST' (RetJSAST x) = [RetJSAST (flatJSAST x)]
flatJSAST' (ExprStmtJSAST x) = [ExprStmtJSAST (flatJSAST x)]
flatJSAST' (StmtExprJSAST x) = [StmtExprJSAST (flatJSAST x)]
flatJSAST' (ProgramJSAST xs) = [ProgramJSAST (concatMap flatJSAST' xs)]
flatJSAST' (CondJSAST x y z) = [CondJSAST (flatJSAST x) (flatJSAST y) (flatJSAST z)]
flatJSAST' (UnaryJSAST x y) = [UnaryJSAST x (flatJSAST y)]
flatJSAST' (BinaryJSAST x y z) = [BinaryJSAST x (flatJSAST y) (flatJSAST z)]

flatJSAST :: JSAST -> JSAST
flatJSAST (BlockJSAST xs) =
  case concatMap flatJSAST' xs of
    [] -> BlockJSAST []
    ys -> BlockJSAST ys
flatJSAST x =
  case flatJSAST' x of
    [] -> BlockJSAST []
    [y] -> y
    xs -> BlockJSAST xs

instance ToJSON JSAST where
  toJSON (DefVarsJSAST xs)
    = object [
        Text.pack "type" .= "VariableDeclaration",
        Text.pack "declarations" .= map (\x -> object [
                                      Text.pack "type" .= "VariableDeclarator",
                                      Text.pack "id" .= IdentJSAST x
                                    ]) xs,
        Text.pack "kind" .= "var"
      ]
  toJSON (AssignJSAST x y)
    = object [
        Text.pack "type" .= "AssignmentExpression",
        Text.pack "operator" .= "=",
        Text.pack "left" .= x,
        Text.pack "right" .= y
      ]
  toJSON (MemberJSAST x s)
    = object [
        Text.pack "type" .= "MemberExpression",
        Text.pack "computed" .= False,
        Text.pack "object" .= x,
        Text.pack "property" .= IdentJSAST s
      ]
  toJSON (ComputedMemberJSAST x y)
    = object [
        Text.pack "type" .= "MemberExpression",
        Text.pack "computed" .= True,
        Text.pack "object" .= x,
        Text.pack "property" .= y
      ]
  toJSON (ObjLitJSAST xs)
    = object [
        Text.pack "type" .= "ObjectExpression",
        Text.pack "properties" .=
          map (\(k,v) -> object [
                          Text.pack "type" .= "Property",
                          Text.pack "key" .= IdentJSAST k,
                          Text.pack "value" .= v,
                          Text.pack "kind" .= "init"
            ]) (HashMap.toList xs)
      ]
  toJSON (FuncLitJSAST params body)
    = object [
        Text.pack "type" .= "FunctionExpression",
        Text.pack "params" .= params,
        Text.pack "body" .= body
      ]
  toJSON (IdentJSAST s)
    = object [
        Text.pack "type" .= "Identifier",
        Text.pack "name" .= s
      ]
  toJSON (StrLitJSAST s)
    = object [
        Text.pack "type" .= "Literal",
        Text.pack "value" .= s
        ]
  toJSON (NumLitJSAST n)
    = object [
        Text.pack "type" .= "Literal",
        Text.pack "value" .= n
      ]
  toJSON (BlockJSAST xs)
    = object [
        Text.pack "type" .= "BlockStatement",
        Text.pack "body" .= xs
      ]
  toJSON (CallJSAST func args)
    = object [
        Text.pack "type" .= "CallExpression",
        Text.pack "callee" .= func,
        Text.pack "arguments" .= args
      ]
  toJSON (RetJSAST x)
    = object [ Text.pack "type" .= "ReturnStatement",
               Text.pack "argument" .= x
              ]
  toJSON (ExprStmtJSAST x)
    = object [ Text.pack "type" .= "ExpressionStatement",
               Text.pack "expression" .= x ]
  toJSON (StmtExprJSAST x)
    = toJSON (CallJSAST (FuncLitJSAST [] x) [])
  toJSON (ProgramJSAST xs)
    = object [
        Text.pack "type" .= "Program",
        Text.pack "body" .= xs
      ]
  toJSON (CondJSAST x y z)
    = object [
        Text.pack "type" .= "ConditionalExpression",
        Text.pack "test" .= x,
        Text.pack "consequent" .= y,
        Text.pack "alternate" .= z
      ]
  toJSON (UnaryJSAST x y)
    = object [
        Text.pack "type" .= "UnaryExpression",
        Text.pack "operator" .= x,
        Text.pack "argument" .= y,
        Text.pack "prefix" .= True
      ]
  toJSON (BinaryJSAST x y z)
    = object [
        Text.pack "type" .= "BinaryExpression",
        Text.pack "operator" .= x,
        Text.pack "left" .= y,
        Text.pack "right" .= z
      ]
