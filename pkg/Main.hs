{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import System.IO.Unsafe
import Control.Exception
import qualified Ormolu
import Reflex.Dom
import GHCJS.DOM
import GHCJS.DOM.Document (getBodyUnchecked)
import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.Types (JSM)
import Data.Text as Text

main :: IO ()
main = mainWidgetInElementByIdMaybe "ormolu-live" $ do
  t <- textArea $ def
  el "pre" $
    el "code" $
      dynText =<<
        foldDyn (\new old -> case new of
          Left _ -> if "..." `Text.isSuffixOf` old then old else old <> "..."
          Right o -> o) ""
          (ormolu . Text.unpack <$> _textArea_input t)

ormolu :: String -> Either String Text
ormolu "" = Right ""
ormolu s =
  either (Left . show) Right $
  unsafePerformIO $
    try @SomeException $
    Ormolu.ormolu Ormolu.defaultConfig "" s

-- | Run a reflex-dom application inside of an existing DOM element with the
-- given ID. If the element does not exist, attach it to the document body.
mainWidgetInElementByIdMaybe :: Text -> (forall x. Widget x ()) -> JSM ()
mainWidgetInElementByIdMaybe eid w =
  withJSContextSingleton $ \jsSing -> do
    doc <- currentDocumentUnchecked
    getElementById doc eid >>= \case
      Nothing -> getBodyUnchecked doc >>= \root -> attachWidget root jsSing w
      Just root -> attachWidget root jsSing w
