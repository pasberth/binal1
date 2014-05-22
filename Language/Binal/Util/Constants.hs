module Language.Binal.Util.Constants where

import qualified Data.HashSet        as HashSet
import qualified Data.HashMap.Strict as HashMap
import           Language.Binal.Types

keywords :: HashSet.HashSet String
keywords = HashSet.fromList
            [ "lambda"
            , "seq"
            , "let"
            ]

primitives :: HashSet.HashSet String
primitives = HashSet.fromList (HashMap.keys initialTypeEnv)

initialVarList :: [Variable]
initialVarList = [2..]

initialTypeEnv :: TypeEnv
initialTypeEnv = HashMap.fromList
                  [ ("str.add@", ArrTy (ListTy [StrTy, StrTy]) StrTy)
                  , ("int.add@", ArrTy (ListTy [IntTy, IntTy]) IntTy)
                  , ("num.add@", ArrTy (ListTy [NumTy, NumTy]) NumTy)
                  , ("apply@", ArrTy (ListTy [ArrTy (VarTy 0) (VarTy 1), VarTy 0]) (VarTy 1))
                  ]

initialPolyEnv :: PolyEnv
initialPolyEnv = HashSet.fromList [0..1]
