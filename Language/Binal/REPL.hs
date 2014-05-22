module Language.Binal.REPL where

import           Control.Monad.Trans
import qualified System.Console.Haskeline  as Haskeline
import qualified Language.Binal.Util       as Util
import qualified Language.Binal.Parser     as P
import qualified Language.Binal.CLI        as CLI

repl :: IO ()
repl = Haskeline.runInputT Haskeline.defaultSettings loop where
  loop :: Haskeline.InputT IO ()
  loop = do
    minput <- Haskeline.getInputLine "Binal < "
    case minput of
      Nothing -> return ()
      Just ":q" -> return ()
      Just input
        | null input -> loop
        | otherwise -> do
          maybeAST <- liftIO (P.parseStringSExp (input ++ "\n"))
          case maybeAST of
            Just ast -> do
              maybeTypedAST <- liftIO (CLI.checkAST ast)
              case maybeTypedAST of
                Just typedAST -> do
                  let msg = "- : " ++ Util.showTy (Util.typeof typedAST)
                  Haskeline.outputStrLn msg
                Nothing -> do
                  return ()
            Nothing -> do
              return ()
          loop
