module Language.Binal.Util.LitKind where

import           Language.Binal.Types

extractSym :: LitKind -> Maybe String
extractSym (SymLit s) = Just s
extractSym _ = Nothing

extractStr :: LitKind -> Maybe String
extractStr (StrLit s) = Just s
extractStr _ = Nothing

extractNum :: LitKind -> Maybe Double
extractNum (NumLit n) = Just n
extractNum _ = Nothing
