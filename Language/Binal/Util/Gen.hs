module Language.Binal.Util.Gen where

indent :: Int -> String -> String
indent n = init . unlines . map (replicate n ' ' ++) . lines
