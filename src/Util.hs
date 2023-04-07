{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Generics.Labels ()
import Data.Text
import Data.Text.IO qualified as T
import Lens.Micro ((^.))
import OpenAI.Type

pprintChat :: [Message] -> IO ()
pprintChat =
  mapM_ (\x -> T.putStrLn $ tshow (x ^. #role) <> ": " <> x ^. #content <> "\n-----------------------------")

tshow :: Show a => a -> Text
tshow = pack . show
