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

instance Show TypedAST where
  show (TyLit lit ty _) = show lit ++ ":" ++ show ty
  show (TyList xs _) = "(" ++ concat (List.intersperse " " (map show xs)) ++ ")"

type Variable = Int

data TyKind
  = VarTy Variable
  | StrTy
  | IntTy
  | NumTy
  | ArrTy TyKind TyKind
  | ListTy [TyKind]
  deriving (Eq)

instance Show TyKind where
  show (VarTy i) = "'_" ++ show i
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

data NotInScope = NotInScope String Where

data Absurd = UnexpectedType TyKind TyKind Where
