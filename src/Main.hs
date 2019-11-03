{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

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
  elAttr "div" ("class" =: "ormolu-output") $ do
    let render OutputState{..} = do
          elAttr "pre" ("class" =: "source-code ormolu-result") $
            el "code" formatted
          case errorMsg of
            Nothing -> pure ()
            Just msg ->
              elAttr "pre" ("class" =: "ormolu-error") $
                el "code" (text (pack msg))
    void . dyn . fmap render =<<
      foldDyn
        update
        (OutputState (pure ()) Nothing)
        (unpack <$> _textArea_input t)

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

data OutputState m = OutputState
  { formatted :: m ()
  , errorMsg :: Maybe String
  }

update :: MonadWidget t m => String -> OutputState m -> OutputState m
update input prevState =
  let OrmoluResult{..} = ormolu input
   in OutputState
        { formatted = maybe (formatted prevState) ghcSyntaxHighlighter ormoluOutput
        , errorMsg = ormoluError
        }

----------------------------------------------------------------------------
-- Formatting
----------------------------------------------------------------------------

data OrmoluResult = OrmoluResult
  { ormoluOutput :: Maybe Text
  , ormoluError :: Maybe String
  }

ormolu :: String -> OrmoluResult
ormolu "" = OrmoluResult
    { ormoluOutput = Just ""
    , ormoluError = Nothing
    }
ormolu s =
    case ormoluWithConfig Ormolu.defaultConfig of
      -- all OK
      Right out -> OrmoluResult
          { ormoluOutput = Just out
          , ormoluError = Nothing
          }
      Left err ->
        case ormoluWithConfig Ormolu.defaultConfig{Ormolu.cfgUnsafe = True} of
          -- can format only with --unsafe
          Right out -> OrmoluResult
              { ormoluOutput = Just out
              , ormoluError = Just err
              }
          -- can't format even with --unsafe
          Left _ -> OrmoluResult
              { ormoluOutput = Nothing
              , ormoluError = Just err
              }
  where
    ormoluWithConfig cfg =
      either (Left . displayException) Right $
      unsafePerformIO $
        try @SomeException $
        Ormolu.ormolu cfg "<input>" s

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
