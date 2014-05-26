module Language.Binal.REPL where

import           Control.Monad.State
import qualified Data.List                 as List
import qualified Data.IORef                as IORef
import qualified System.Console.Haskeline  as Haskeline
import qualified Language.Binal.Util       as Util
import qualified Language.Binal.PrettyPrint as PP
import qualified Language.Binal.Parser     as P
import qualified Language.Binal.Verifier as V

repl :: IO ()
repl = Haskeline.runInputT Haskeline.defaultSettings start where
  start = do
    ref1 <- liftIO (IORef.newIORef Util.primitives)
    ref2 <- liftIO (IORef.newIORef (Util.initialTypeEnv, Util.initialVarList, [], Util.initialPolyEnv))
    loop ref1 ref2
  loop ref1 ref2 = do
    minput <- Haskeline.getInputLine "Binal < "
    case minput of
      Nothing -> return ()
      Just ":q" -> return ()
      Just input
        | null input -> loop ref1 ref2
        | otherwise -> do
          maybeAST <- liftIO (P.parseStringSExp (input ++ "\n"))
          case maybeAST of
            Just ast -> do
              case V.examineForms ast of
                [] -> do
                  state1 <- liftIO (IORef.readIORef ref1)
                  case runState (V.examineNames' ast) state1 of
                    ([], newState1) -> do
                      state2 <- liftIO (IORef.readIORef ref2)

                      let (typedAST, newState2@(_, _, constraints, _)) = runState (V.inferType' ast) state2
                      let absurds = List.nub (V.cantUnify (reverse constraints))
                      let typedAST' =  Util.mapTyKind (V.unify (reverse constraints) . Util.flatListTy) typedAST
                      case absurds of
                        [] -> do
                          liftIO $ do
                            IORef.writeIORef ref1 newState1
                            IORef.writeIORef ref2 newState2
                          let msg = "- : " ++ Util.showTy (Util.typeof typedAST')
                          Haskeline.outputStrLn msg
                        errs -> do
                          liftIO (mapM_ PP.prettyANSIAbsurd errs)
                    (errs, _) -> do
                      liftIO (mapM_ PP.prettyANSINotInScope errs)
                errs -> do
                  liftIO (mapM_ PP.prettyANSISyntaxError errs)
            Nothing -> return ()
          loop ref1 ref2
