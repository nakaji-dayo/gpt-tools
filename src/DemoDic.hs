{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module DemoDic where

import OpenAI.Client
import OpenAI.Type
import Data.Text (Text, pack)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe, mapMaybe)
import Lens.Micro (ix, (^?))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String.Interpolate
import Text.Pretty.Simple (pPrint)
import Util

gptprompt :: String
gptprompt = [i|
質問に回答して下さい。ステップバイステップで考えて下さい。
次のいづれかのコマンドで回答して下さい(search, answer)。
コマンドにはフォーマットが決まっています。フォーマットに含まれない文章は不要です。

# 知らない単語が含まれる場合:
```
search: {単語を入力}
```

# 正解がわかり回答できる場合:
```
answer: {解答を入力}
```
|]

dicDemo1 :: IO ()
dicDemo1 = do
  key <- fromMaybe (error "OPENAI_KEY required") <$> lookupEnv "OPENAI_KEY"
  run key dummyDic "hvguy8fはどこの都道府県の食べ物ですか?"

dicDemo :: FilePath -> String ->  IO ()
dicDemo f q = do
  key <- fromMaybe (error "OPENAI_KEY required") <$> lookupEnv "OPENAI_KEY"
  d <- readDic f
  run key d (pack q)

run :: String -> [(Text, Text)] -> Text -> IO ()
run key dic q = talk initMsg
  where
    initMsg =
      [ Message System (pack gptprompt)
      , Message User q
      ]
    talk msgs = do
      pprintChat msgs
      res <- callAPI (pack key) $ defaultGPT35{ messages = msgs }
      case res of
        Right x -> do
          case x ^? #choices . ix 0 . #message . #content of
            Just c -> talk $ msgs ++ [Message Assistant c, usrAns c]
            _ -> putStrLn "exit"
        Left e -> print e
    parseCmd p = fmap T.strip . mapMaybe (T.stripPrefix p)
    search = parseCmd "search:"
    answer = parseCmd "answer:"
    usrAns msg =
      let ls = T.lines msg
      in case (search ls, answer ls) of
        ([], []) -> Message User "フォーマットを守って下さい"
        (xs, []) -> let ws = fmap (\k -> "- " <> k <> ": " <> lookupDic k) xs
                    in Message User ("次の用語集を使って回答して下さい: \n" <> T.unlines ws <> "\n\n" <> q)
        ([], _) -> Message User "解答内の未知の単語を再度searchして下さい。"
        _ -> Message User "コマンドは一つづつ使って下さい。"
    lookupDic k = fromMaybe "未知の単語です" $ lookup k dic


dummyDic :: [(Text, Text)]
dummyDic =
  [ ("hvguy8f", "hvguy8fはbvjgvuの別名")
  , ("bvjgvu", "石川県の野菜")
  ]

readDic :: FilePath -> IO [(Text, Text)]
readDic f = do
  c <- T.readFile f
  let (v, mk, d) = foldl build ("", Nothing, []) (T.lines c)
  pure $ maybe d (\k -> (T.strip k, v):d) mk
  where
    build :: (Text, Maybe Text, [(Text, Text)]) -> Text -> (Text, Maybe Text, [(Text, Text)])
    build (v, Just k, acc) (T.stripPrefix "##" -> Just x) = ("", Just x, (T.strip k, v): acc)
    build (_, _, acc) (T.stripPrefix "##" -> Just x) = ("", Just x, acc)
    build (v, k, acc) x = (v <> x, k, acc)

test :: IO ()
test = readDic "dic.txt" >>= pPrint
