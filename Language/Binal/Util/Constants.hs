module Language.Binal.Util.Constants where

import qualified Data.HashSet        as HashSet

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

infiniteVarList :: [Int]
infiniteVarList = [0..]
