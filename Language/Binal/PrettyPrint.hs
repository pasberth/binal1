module Language.Binal.PrettyPrint where

import qualified System.IO
import qualified System.Console.ANSI as ANSI
import           Language.Binal.Types
import qualified Language.Binal.Util as Util

prettyANSIError :: IO ()
prettyANSIError = do
  ANSI.hSetSGR System.IO.stderr [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Red, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStr System.IO.stderr "error: "
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]

prettyANSIWarning :: IO ()
prettyANSIWarning = do
  ANSI.hSetSGR System.IO.stderr [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Magenta, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStr System.IO.stderr "warning: "
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]

prettyANSIWhere :: Where -> IO ()
prettyANSIWhere pos = do
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  case pos of
    AtFile path lineno columnno _ _ _ -> do
      let s = path ++ ":" ++ show (lineno + 1) ++ ":" ++ show (columnno + 1) ++ ": "
      System.IO.hPutStr System.IO.stderr s
    AtLine lineno columnno _ _ _ -> do
      let s = show (lineno + 1) ++ ":" ++ show (columnno + 1) ++ ": "
      System.IO.hPutStr System.IO.stderr s
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]

prettyANSIHintLine :: Where -> IO ()
prettyANSIHintLine pos = do
  let (lineno, columnno, linenoOfEnd, columnnoOfEnd', contents) =
        case pos of
          AtFile _ a b c d e ->
            (a, b, c, d, e)
          AtLine a b c d e ->
            (a, b, c, d, e)
  let columnnoOfEnd = if lineno == linenoOfEnd
                        then columnnoOfEnd'
                        else length contents
  let prefix = take columnno contents
  let problemToken = drop columnno (take columnnoOfEnd contents)
  let suffix = drop columnnoOfEnd contents
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  System.IO.hPutStr System.IO.stderr prefix
  ANSI.hSetSGR System.IO.stderr [ANSI.SetUnderlining ANSI.SingleUnderline]
  System.IO.hPutStr System.IO.stderr problemToken
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  System.IO.hPutStr System.IO.stderr suffix
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]

prettyANSIStyleError :: StyleError -> IO ()
prettyANSIStyleError (UnexpectedEOFWhileReading pos) = do
  let msg = "unexpected end of file; maybe you forgot a `)'."
  prettyANSIWhere pos
  prettyANSIError
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  prettyANSIHintLine pos
prettyANSIStyleError (MismatchIndent before _) = do
  let msg = "matching `)' was not found within indented codes; maybe you forgot a `)'."
  prettyANSIWhere before
  prettyANSIWarning
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  prettyANSIHintLine before

prettyANSISyntaxError :: SyntaxError -> IO ()
prettyANSISyntaxError (KeywordUsedAsVariable kwd pos) = do
  let msg = "keyword `" ++ kwd ++ "' used as a variable"
  prettyANSIWhere pos
  prettyANSIError
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
prettyANSISyntaxError (UnexpectedArity i j pos) = do
  let msg = "wrong number of forms(" ++ show j ++ " for " ++ show i ++ ")"
  prettyANSIWhere pos
  prettyANSIError
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  prettyANSIHintLine pos
prettyANSISyntaxError (Malformed pos) = do
  let msg = "malformed syntax"
  prettyANSIWhere pos
  prettyANSIError
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  prettyANSIHintLine pos

prettyANSINotInScope :: NotInScope -> IO ()
prettyANSINotInScope (NotInScope ident pos) = do
  let msg = "not in scope `" ++ ident ++ "'"
  prettyANSIWhere pos
  prettyANSIError
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  prettyANSIHintLine pos

prettyANSIAbsurd :: Absurd -> IO ()
prettyANSIAbsurd (UnexpectedType expected actual pos) = do
  let (x, y) = Util.showTy2 expected actual
  let msg = "couldn't match type: expected " ++ x ++ " actual " ++ y
  prettyANSIWhere pos
  prettyANSIError
  ANSI.hSetSGR System.IO.stderr [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  System.IO.hPutStrLn System.IO.stderr msg
  ANSI.hSetSGR System.IO.stderr [ANSI.Reset]
  prettyANSIHintLine pos
