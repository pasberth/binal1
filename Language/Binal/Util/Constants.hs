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
            ]

primitives :: HashSet.HashSet String
primitives = HashSet.fromList ("true":"false":HashMap.keys initialTypeEnv)

initialVarList :: [Variable]
initialVarList = [1..]

initialTypeEnv :: TypeEnv
initialTypeEnv = HashMap.fromList
                  [ ("str.add", ArrTy (ListTy [StrTy, StrTy]) StrTy)
                  , ("num.add", ArrTy (ListTy [NumTy, NumTy]) NumTy)
                  , ("require", ArrTy StrTy (VarTy 0))
                  ]

initialPolyEnv :: PolyEnv
initialPolyEnv = HashSet.fromList [0]
