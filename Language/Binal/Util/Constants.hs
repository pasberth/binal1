module Language.Binal.Util.Constants where

import qualified Data.HashSet        as HashSet
import qualified Data.HashMap.Strict as HashMap
import           Language.Binal.Types

keywords :: HashSet.HashSet String
keywords = HashSet.fromList
            [ "^"
            , "seq"
            , "let"
            , "letrec"
            , "match"
            , "object"
            , "."
            , ":="
            , "object"
            , "assume"
            , "cond"
            , "val"
            ]

primitives :: HashSet.HashSet String
primitives = HashSet.fromList ("true":"false":HashMap.keys initialTypeEnv)

initialVarList :: [Variable]
initialVarList = [3..]

initialTypeEnv :: TypeEnv
initialTypeEnv = HashMap.fromList
                  [ ("str.add", ArrTy (ListTy [StrTy, StrTy]) StrTy)
                  , ("num.add", ArrTy (ListTy [NumTy, NumTy]) NumTy)
                  , ("require", ArrTy StrTy (VarTy 0))
                  , ("mutable", ArrTy (VarTy 1) (MutableTy (VarTy 1)))
                  , ("unmutable", ArrTy (MutableTy (VarTy 2)) (VarTy 2))
                  ]

initialPolyEnv :: PolyEnv
initialPolyEnv = HashSet.fromList [0..2]
