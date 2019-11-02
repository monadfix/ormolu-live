{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import System.IO.Unsafe
import Control.Exception
import qualified Ormolu
import Reflex.Dom
import qualified Data.Text as T

main :: IO ()
main = mainWidget $ do
  t <- textArea $ def
  el "pre" $
    el "code" $
      dynText =<<
        foldDyn (\new old -> case new of
          Left _ -> if "..." `T.isSuffixOf` old then old else old <> "..."
          Right o -> o) ""
          (ormolu . T.unpack <$> _textArea_input t)

ormolu :: String -> Either String T.Text
ormolu "" = Right ""
ormolu s =
  either (Left . show) Right $
  unsafePerformIO $
    try @SomeException $
    Ormolu.ormolu Ormolu.defaultConfig "" s
