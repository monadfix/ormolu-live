{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import System.IO.Unsafe
import Control.Exception
import Data.Functor
import qualified Ormolu
import GHC.SyntaxHighlighter
import Reflex.Dom
import GHCJS.DOM
import GHCJS.DOM.Document (getBodyUnchecked)
import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.Types (JSM)
import Data.Text as Text

main :: IO ()
main = mainWidgetInElementByIdMaybe "ormolu-live" $ do
  t <- textArea $ def
  elAttr "pre" ("class" =: "source-code") $
    elAttr "code" ("class" =: "language-haskell") $ do
      out <-
        fmap render <$>
        foldDyn
          update
          (Good (pure ()))
          (unpack <$> _textArea_input t)
      void (dyn out)

render :: MonadWidget t m => OutputState t m -> m ()
render = \case
  Good output -> output
  Broken output err -> text (pack err <> "\n\n") >> output

-- | Run a reflex-dom application inside of an existing DOM element with the
-- given ID. If the element does not exist, attach it to the document body.
mainWidgetInElementByIdMaybe :: Text -> (forall x. Widget x ()) -> JSM ()
mainWidgetInElementByIdMaybe eid w =
  withJSContextSingleton $ \jsSing -> do
    doc <- currentDocumentUnchecked
    getElementById doc eid >>= \case
      Nothing -> getBodyUnchecked doc >>= \root -> attachWidget root jsSing w
      Just root -> attachWidget root jsSing w

----------------------------------------------------------------------------
-- Processing
----------------------------------------------------------------------------

data OutputState t m
  = Good { currentOutput :: m () }
  | Broken { previousOutput :: m ()
           , currentError :: String }

update :: MonadWidget t m => String -> OutputState t m -> OutputState t m
update input state =
  case (state, ormolu input) of
    (_, Right val) -> Good (ghcSyntaxHighlighter val)
    (Good output, Left err) -> Broken output err
    (Broken output _, Left err) -> Broken output err

----------------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------------

ormolu :: String -> Either String Text
ormolu "" = Right ""
ormolu s =
  either (Left . show) Right $
  unsafePerformIO $
    try @SomeException $
    Ormolu.ormolu Ormolu.defaultConfig "" s

----------------------------------------------------------------------------
-- Highlighting
----------------------------------------------------------------------------

ghcSyntaxHighlighter :: MonadWidget t m => Text -> m ()
ghcSyntaxHighlighter code = case tokenizeHaskell code of
    Nothing -> text code
    Just tokens -> mapM_ tokenToElement tokens

tokenToElement :: MonadWidget t m => (Token, Text) -> m ()
tokenToElement (token, txt) =
  case tokenClass token of
    Nothing -> el "span" (text txt)
    Just class_ -> elAttr "span" ("class" =: class_ <> "data-text" =: txt) (text txt)

tokenClass :: Token -> Maybe Text
tokenClass = \case
  KeywordTok -> Just "kw"
  PragmaTok -> Just "pr"
  SymbolTok -> Just "sy"
  VariableTok -> Just "va"
  ConstructorTok -> Just "cr"
  OperatorTok -> Just "op"
  CharTok -> Just "ch"
  StringTok -> Just "st"
  IntegerTok -> Just "it"
  RationalTok -> Just "ra"
  CommentTok -> Just "co"
  OtherTok -> Just "ot"
  SpaceTok -> Nothing
