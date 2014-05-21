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
          _ <- B.checkAST ast
          return ()
        Nothing -> System.Exit.exitFailure
    _ -> System.Exit.exitFailure
