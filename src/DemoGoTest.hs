{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module DemoGoTest where

import ChatM (systemM, userM, runChat)
import qualified Data.Text as T
import Data.Maybe
import System.Environment
import qualified Data.Text.IO as T
import Text.Pretty.Simple (pPrint)
import Lens.Micro ((^.))
import Data.Text (pack, unpack, Text)
import Data.Attoparsec.Text
import System.Process (readProcess, runProcess, waitForProcess)
import Control.Monad
import Util (pprintChat)
import System.FilePath (takeDirectory)

goTestDemo :: FilePath -> IO ()
goTestDemo f = do
  key <- fromMaybe (error "OPENAI_KEY required") <$> lookupEnv "OPENAI_KEY"
  code <- T.readFile f
  T.putStrLn code
  res <- runChat key $ do
    _ <- systemM "以降では、あなたはコードのみを回答して下さい。コードの解説は不要です。"
    userM $ T.unlines
      [ "次のGoでのテストコードに対応する実装を回答してください。"
      , "```"
      , code
      , "```"
      ]
  let impl = case parseAns (res ^. #content) of
        Right x -> pack x
        Left e -> error e
  let implf = unpack $ T.replace "_test" "" (pack f)
  pPrint f
  T.putStrLn impl
  T.writeFile implf impl
  putStrLn "Run a test?(y/n)"
  yn <- getLine
  when (yn == "y") $ do
    run "go" ["mod", "tidy"]
    e <- run "go" ["test"]
    pPrint e
  where
    run cmd args = runProcess cmd args (Just (takeDirectory f)) Nothing Nothing Nothing Nothing >>= waitForProcess

parseAns = parseOnly pCode

pCode :: Parser String
pCode =  manyTill anyChar (string "```") >> manyTill anyChar (string "```")
