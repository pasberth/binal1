module Language.Binal.StyleGuidance where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State
import qualified Text.Trifecta         as T
import qualified Text.Trifecta.Delta   as D
import           Language.Binal.Types
import qualified Language.Binal.Parser as P

type StyleGuidance a = StateT [Where] T.Parser a

isStyleError :: StyleError -> Bool
isStyleError (UnexpectedEOFWhileReading _) = True
isStyleError (ExtraCloseParenthesis _) = True
isStyleError (BadToken _) = True
isStyleError (MismatchIndent _ _) = False

isStyleWarning :: StyleError -> Bool
isStyleWarning (UnexpectedEOFWhileReading _) = False
isStyleWarning (ExtraCloseParenthesis _) = False
isStyleWarning (BadToken _) = False
isStyleWarning (MismatchIndent _ _) = True

styleErrors :: [StyleError] -> [StyleError]
styleErrors = filter isStyleError

examineStyleWithinProgram :: StyleGuidance [StyleError]
examineStyleWithinProgram = do
  errs1 <- examineStyle
  errs2 <- concat <$> many (T.spaces *> examineExtraCloseParenthesis <* T.spaces)
  return (errs1 ++ errs2)

examineStyle :: StyleGuidance [StyleError]
examineStyle = concat <$> many (T.spaces *> examineStyle1 <* T.spaces)

examineStyle1 :: StyleGuidance [StyleError]
examineStyle1 = examineStyleAtom <|> examineStyleList <|> examineBadToken

examineStyleAtom :: StyleGuidance [StyleError]
examineStyleAtom = lift P.atom >> return []

examineExtraCloseParenthesis :: StyleGuidance [StyleError]
examineExtraCloseParenthesis = do
  (pos, _) <- lift (P.withPosition (T.char ')'))
  return [ExtraCloseParenthesis pos]

examineBadToken :: StyleGuidance [StyleError]
examineBadToken = do
  (pos, _) <- lift (P.withPosition (some (T.noneOf " \n()")))
  return [BadToken pos]

examineStyleList :: StyleGuidance [StyleError]
examineStyleList = do
  stack <- get
  let before = case stack of
                [] -> Nothing
                (x:_) -> Just x
  (begin, _) <- lift (P.withPosition (T.char '('))
  let mkIndent =
        case (before, begin) of
          (Just (AtFile _ beforeL _ _ _ _), AtFile _ beginL _ _ _ _)
            | beforeL == beginL -> False
            | otherwise -> True
          (Just (AtLine beforeL _ _ _ _), AtLine beginL _ _ _ _)
            | beforeL == beginL -> False
            | otherwise -> True
          _ -> True
  when mkIndent $ do
    modify (begin:)
  mismatches1 <- examineStyle
  matchingParen <- lift (P.withPosition (optional (T.char ')')))
  when mkIndent $ do
    modify tail
  let mismatches2
        = case (before, begin) of
          (Just b@(AtFile _ beforeL beforeC _ _ _), AtFile _ beginL beginC _ _ _)
            | beforeL == beginL -> do
              []
            | beforeC < beginC -> do
              []
            | beforeC >= beginC -> do
              [MismatchIndent b begin]
          (Just b@(AtLine beforeL beforeC _ _ _), AtLine beginL beginC _ _ _)
            | beforeL == beginL -> do
              []
            | beforeC < beginC -> do
              []
            | beforeC >= beginC -> do
              [MismatchIndent b begin]
          _ -> []
  let mismatches3
        = case matchingParen of
            (_, Just _) -> []
            (pos, Nothing) -> [UnexpectedEOFWhileReading pos]
  return (mismatches1 ++ mismatches2 ++ mismatches3)

examineStyleFromFile :: FilePath -> IO (Maybe [StyleError])
examineStyleFromFile path = T.parseFromFile (evalStateT (examineStyleWithinProgram <* T.eof) []) path

examineStyleString :: String -> IO (Maybe [StyleError])
examineStyleString str = do
  case T.parseString (evalStateT (examineStyleWithinProgram <* T.eof) []) (D.Columns 0 0) str of
    T.Success x -> return (Just x)
    T.Failure doc -> do
      putStrLn (show doc)
      return Nothing
