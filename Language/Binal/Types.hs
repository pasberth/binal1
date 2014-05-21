module Language.Binal.Types where

import qualified Data.List as List

data Where
  = AtFile      -- inputs from a named file.
      FilePath  -- ^ filepath
      Int       -- ^ lineno of start
      Int       -- ^ columnno of start
      Int       -- ^ lineno of end
      Int       -- ^ columnno of end
      String    -- ^ the contents of the current line (from the beginning of the line)
  | AtLine      -- inputs from an anonymous stream. e.g. stdin
      Int       -- ^ lineno
      Int       -- ^ columnno
      Int       -- ^ lineno of end
      Int       -- ^ columnno of end
      String    -- ^ the contents of the current line (from the beginning of the line) 
  deriving (Show)

data LitKind
  = SymLit String
  | StrLit String
  | IntLit Int
  | NumLit Double

instance Show LitKind where
  show (SymLit s) = s
  show (StrLit s) = show s
  show (IntLit i) = show i
  show (NumLit i) = show i

data AST
  = Lit LitKind Where
  | List [AST] Where

instance Show AST where
  show (Lit lit _) = show lit
  show (List xs _) = "(" ++ concat (List.intersperse " " (map show xs)) ++ ")"

data TypedAST
  = TyLit LitKind TyKind Where
  | TyList [TypedAST] Where
  | TyApp TypedAST [TypedAST] TyKind Where
  | TyAbs TypedAST TypedAST Where
  | TySeq [TypedAST] Where
  | TyLet TypedAST TypedAST Where

instance Show TypedAST where
  show (TyLit lit ty _) = show lit ++ ":" ++ show ty
  show (TyList xs _) = "(" ++ concat (List.intersperse " " (map show xs)) ++ ")"
  show (TyApp func args _ _) = "(" ++ concat (List.intersperse " " (map show (func:args))) ++ ")"
  show (TyAbs params body _) = "(" ++ concat (List.intersperse " " ("lambda":(map show [params,body]))) ++ ")"
  show (TySeq exprs _) = "(" ++ concat (List.intersperse " " ("seq":(map show exprs))) ++ ")"
  show (TyLet pattern value _) = "(" ++ concat (List.intersperse " " ("let":(map show [pattern,value]))) ++ ")"

data TyKind
  = VarTy Int -- type variable
  | PVarTy Int -- polymorphic type variable
  | StrTy
  | IntTy
  | NumTy
  | ArrTy TyKind TyKind
  | ListTy [TyKind]
  deriving (Eq)

instance Show TyKind where
  show (VarTy i) = "'_" ++ show i
  show (PVarTy i) = "'" ++ show i
  show StrTy = "string"
  show IntTy = "int"
  show NumTy = "number"
  show (ArrTy src tgt) = "(-> " ++ show src ++ " " ++ show tgt ++ ")"
  show (ListTy xs) = "(" ++ concat (List.intersperse " " (map show xs)) ++ ")"

data SyntaxError
  = KeywordUsedAsVariable
      String -- ^ keyword
      Where
  | UnexpectedArity
      Int -- ^ expected
      Int -- ^ actual
      Where
  deriving (Show)

data NotInScope = NotInScope String Where
  deriving (Show)

data Absurd = UnexpectedType TyKind TyKind Where
