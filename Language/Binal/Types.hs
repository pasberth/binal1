module Language.Binal.Types where

import           Control.Monad.State
import qualified Data.List as List
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap

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
  deriving (Eq)

data LitKind
  = SymLit String
  | StrLit String
  | NumLit Double
  | BoolLit Bool
  deriving (Eq)

instance Show LitKind where
  show (SymLit s) = s
  show (StrLit s) = show s
  show (NumLit i) = show i
  show (BoolLit True) = "true"
  show (BoolLit False) = "false"

data AST
  = Lit LitKind Where
  | List [AST] Where

instance Show AST where
  show (Lit lit _) = show lit
  show (List xs _) = "(" ++ concat (List.intersperse " " (map show xs)) ++ ")"

data TypedAST
  = TyLit LitKind TyKind Where
  | TyList [TypedAST] TyKind Where

instance Show TypedAST where
  show (TyLit lit ty _) = show lit ++ ":" ++ show ty
  show (TyList xs ty _) = "(" ++ concat (List.intersperse " " (map show xs)) ++ "):" ++ show ty

type Variable = Int

type TypeEnv = HashMap.HashMap String TyKind
type PolyEnv = HashSet.HashSet Variable

type TypeInferer a = State (TypeEnv, [Variable], [Constraint], PolyEnv) a

data TyKind
  = VarTy Variable
  | RecTy Variable TyKind
  | LitTy LitKind
  | SymTy
  | StrTy
  | NumTy
  | BoolTy
  | ArrTy TyKind TyKind
  | ListTy [TyKind]
  | EitherTy [TyKind]
  | ObjectTy (HashSet.HashSet Variable) (HashMap.HashMap String TyKind)
  | MutableTy TyKind
  deriving (Eq)

instance Show TyKind where
  show (VarTy i) = "'_" ++ show i
  show (RecTy i ty) = "(recur '" ++ show i ++ " " ++ show ty ++ ")"
  show (LitTy lit) = show lit
  show SymTy = "symbol"
  show StrTy = "string"
  show NumTy = "number"
  show BoolTy = "bool"
  show (ArrTy src tgt) = "(-> " ++ show src ++ " " ++ show tgt ++ ")"
  show (ListTy xs) = "(" ++ unwords (map show xs) ++ ")"
  show (EitherTy xs) = "(| " ++ unwords (map show xs) ++ ")"
  show (ObjectTy _ m) = "(obj " ++ showm ++ ")" where
    showm = unwords (concatMap (\(key, value) -> [key, show value]) (HashMap.toList m))
  show (MutableTy ty) = "(mutable " ++ show ty ++ ")"

data StyleError
  = UnexpectedEOFWhileReading
      Where
  | ExtraCloseParenthesis
      Where
  | BadToken
      Where
  | MismatchIndent
      Where
      Where

data SyntaxError
  = KeywordUsedAsVariable
      String -- ^ keyword
      Where
  | UnexpectedArity
      Int -- ^ expected
      Int -- ^ actual
      Where
  | Malformed
      Where

data NotInScope = NotInScope String Where

data Constraint
  = Equal TyKind TyKind Absurd
  | Subtype TyKind TyKind Absurd

data Absurd = UnexpectedType TyKind TyKind Where
  deriving (Eq)
