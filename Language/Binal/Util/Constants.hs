module Language.Binal.Util.Constants where

import qualified Data.HashSet        as HashSet
import           Language.Binal.Types

keywords :: HashSet.HashSet String
keywords = HashSet.fromList
            [ "lambda"
            , "seq"
            , "let"
            ]

primitives :: HashSet.HashSet String
primitives = HashSet.fromList
              [ "str.add@"
              , "int.add@"
              , "num.add@"
              , "apply@"
              ]

infiniteVarList :: [Variable]
infiniteVarList = [0..]
