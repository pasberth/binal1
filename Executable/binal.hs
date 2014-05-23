module Main where

import qualified System.Environment
import qualified System.Exit
import qualified Language.Binal        as B

main :: IO ()
main = do
  argv <- System.Environment.getArgs
  case argv of
    [] -> B.repl
    (path:[]) -> do
      maybeAst <- B.parseFromFile path
      case maybeAst of
        Just ast -> do
          maybeTypedAST <- B.checkAST ast
          case maybeTypedAST of
            Just typedAST -> do
              putStrLn (B.generateString typedAST)
              --putStrLn (B.generateInterface typedAST)
            Nothing -> do
              return ()
        Nothing -> System.Exit.exitFailure
    _ -> System.Exit.exitFailure
