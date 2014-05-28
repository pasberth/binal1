module Language.Binal.Parser where

import           Control.Applicative
import qualified Data.ByteString.UTF8  as BS
import qualified Text.Trifecta         as T
import qualified Text.Trifecta.Delta   as D
import           Language.Binal.Types

pathOfDelta :: D.Delta -> Maybe FilePath
pathOfDelta (D.Directed path _ _ _ _) = Just (BS.toString path)
pathOfDelta _ = Nothing

posOfDelta :: D.Delta -> (Int, Int)
posOfDelta (D.Directed _ lineno columnno _ _) =
  (fromInteger (toInteger lineno), fromInteger (toInteger columnno))
posOfDelta (D.Lines lineno columnno _ _) =
  (fromInteger (toInteger lineno), fromInteger (toInteger columnno))
posOfDelta (D.Columns columnno _) =
  (0, fromInteger (toInteger columnno))
posOfDelta (D.Tab _ _ _) =
  (0, 0)

withPosition :: T.Parser a -> T.Parser (Where, a)
withPosition p = do
  contents <- BS.toString <$> T.line
  startD <- T.position
  x <- p
  endD <- T.position
  let start = posOfDelta startD
  let end = posOfDelta endD
  case pathOfDelta startD of
    Just path ->
      return (AtFile path (fst start) (snd start) (fst end) (snd end) contents, x)
    Nothing ->
      return (AtLine (fst start) (snd start) (fst end) (snd end) contents, x)

letter :: T.Parser Char
letter = T.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

specialInitial :: T.Parser Char
specialInitial = T.oneOf "!$%&*/:<=>?^_~.@+-'|"

specialSubsequent :: T.Parser Char
specialSubsequent = T.oneOf ""

digit :: T.Parser Char
digit = T.oneOf "0123456789"

initial :: T.Parser Char
initial = letter <|> specialInitial

subsequent :: T.Parser Char
subsequent = initial <|> digit <|> specialSubsequent

symLit :: T.Parser AST
symLit = do
  (pos, (hd, tl)) <- withPosition ((,) <$> initial <*> many subsequent)
  return (Lit (SymLit (hd:tl)) pos)

strLit :: T.Parser AST
strLit = do
  (pos, str) <- withPosition T.stringLiteral
  return (Lit (StrLit str) pos)

numLit :: T.Parser AST
numLit = do
  (pos, intOrDbl) <- withPosition T.integerOrDouble
  case intOrDbl of
    Left int -> return (Lit (NumLit (realToFrac int)) pos)
    Right dbl -> return (Lit (NumLit dbl) pos)

list :: T.Parser AST
list = do
  (pos, xs) <- withPosition (T.char '(' *> (many (T.spaces *> sexp <* T.spaces)) <* T.char ')')
  return (List xs pos)

atom :: T.Parser AST
atom = strLit <|> T.try numLit <|> symLit

sexp :: T.Parser AST
sexp = list <|> atom

program :: T.Parser AST
program = do
  (pos, xs) <- withPosition (many (T.spaces *> sexp <* T.spaces))
  return (List (Lit (SymLit "seq") pos:xs) pos)

parseFromFile :: FilePath -> IO (Maybe AST)
parseFromFile path = T.parseFromFile (program <* T.eof) path

parseString :: String -> IO (Maybe AST)
parseString str = do
  case T.parseString (program <* T.eof) (D.Columns 0 0) str of
    T.Success x -> return (Just x)
    T.Failure doc -> do
      putStrLn (show doc)
      return Nothing

parseStringSExp :: String -> IO (Maybe AST)
parseStringSExp str = do
  case T.parseString (T.spaces *> sexp <* T.spaces <* T.eof) (D.Columns 0 0) str of
    T.Success x -> return (Just x)
    T.Failure doc -> do
      putStrLn (show doc)
      return Nothing
