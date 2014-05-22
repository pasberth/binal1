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
initialVarList = [0..]

initialTypeEnv :: TypeEnv
initialTypeEnv = HashMap.fromList
                  [ ("str.add@", ArrTy (ListTy [StrTy, StrTy]) StrTy)
                  , ("int.add@", ArrTy (ListTy [IntTy, IntTy]) IntTy)
                  , ("num.add@", ArrTy (ListTy [NumTy, NumTy]) NumTy)
                  ]

initialPolyEnv :: PolyEnv
initialPolyEnv = HashSet.fromList []
