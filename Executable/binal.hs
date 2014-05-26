module Main where

import           Control.Applicative
import qualified System.Environment
import qualified System.Exit
import qualified System.FilePath.Posix
import qualified Language.Binal        as B

main :: IO ()
main = do
  argv <- System.Environment.getArgs
  case argv of
    [] -> B.repl
    (path:[]) -> do
      errs <- maybe [] id <$> B.examineStyleFromFile path
      mapM_ B.prettyANSIStyleError errs
      case B.styleErrors errs of
        [] -> do
          maybeAst <- B.parseFromFile path
          case maybeAst of
            Just ast -> do
              maybeTypedAST <- B.checkAST ast
              case maybeTypedAST of
                Just typedAST -> do
                  let cleanPath = System.FilePath.Posix.dropExtension path
                  let jsonPath = System.FilePath.Posix.addExtension cleanPath "json"
                  let binaliPath = System.FilePath.Posix.addExtension cleanPath "binali"
                  writeFile jsonPath (B.generateString typedAST)
                  writeFile binaliPath (B.generateInterface typedAST)
                Nothing -> do
                  return ()
            Nothing -> System.Exit.exitFailure
        _ -> System.Exit.exitFailure
    _ -> System.Exit.exitFailure
