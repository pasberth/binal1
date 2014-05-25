module Language.Binal.Generator.Util where

import qualified Data.HashMap.Strict as HashMap

replaceCharacters :: HashMap.HashMap Char String
replaceCharacters
  = HashMap.fromList [
      ('-', "_"),
      ('.', "_DOT_"),
      (':', "_COLON_"),
      ('+', "_PLUS_"),
      ('>', "_GT_"),
      ('<', "_LT_"),
      ('=', "_EQ_"),
      ('~', "_TILDE_"),
      ('!', "_BANG_"),
      ('@', "_CIRCA_"),
      ('#', "_SHARP_"),
      ('\'', "_SINGLEQUOTE_"),
      ('"', "_DOUBLEQUOTE_"),
      ('%', "_PERCENT_"),
      ('^', "_CARET_"),
      ('&', "_AMPERSAND_"),
      ('*', "_STAR_"),
      ('|', "_BAR_"),
      ('{', "_LBRACE_"),
      ('}', "_RBRACE_"),
      ('[', "_LBRACK_"),
      (']', "_RBRACK_"),
      ('/', "_SLASH_"),
      ('\\', "_BSLASH_"),
      ('?', "_QMARK_")
    ]

toJSSafeSymbol :: String -> String
toJSSafeSymbol = concatMap (\ch -> case HashMap.lookup ch replaceCharacters of
                              Just s -> s
                              Nothing -> [ch])
