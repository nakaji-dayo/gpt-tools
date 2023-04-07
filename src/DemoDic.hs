{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module DemoDic where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.String.Interpolate
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lens.Micro (ix, (^?), (^.))
import OpenAI.Client
import OpenAI.Type
import System.Environment (lookupEnv)
import Util
import ChatM (msgsM, lastM, userM, Chat, runChat, ppMagsM)
import Control.Monad (forever)

promptDic :: String
promptDic =
  [i|
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
  runChat key $ run dummyDic "hvguy8fはどこの都道府県の食べ物ですか?"

dicDemo :: FilePath -> String -> IO ()
dicDemo f q = do
  key <- fromMaybe (error "OPENAI_KEY required") <$> lookupEnv "OPENAI_KEY"
  d <- readMDDic f
  runChat key $ run d (pack q)

run ::  [(Text, Text)] -> Text -> Chat ()
run dic q = do
  msgsM initMsg
  forever talk
  where
    initMsg =
      [ Message System (pack promptDic),
        Message User q
      ]
    talk = do
      msg <- lastM
      let ans = usrAns (msg ^. #content)
      userM ans
      ppMagsM
    parseCmd p = fmap T.strip . mapMaybe (T.stripPrefix p)
    search = parseCmd "search:"
    answer = parseCmd "answer:"
    usrAns msg =
      let ls = T.lines msg
       in case (search ls, answer ls) of
            ([], []) -> "フォーマットを守って下さい"
            (xs, []) ->
              let ws = fmap (\k -> "- " <> k <> ": " <> lookupDic k) xs
               in "次の用語集を使って回答して下さい: \n" <> T.unlines ws
            ([], _) -> "解答内の未知の単語を再度searchして下さい。"
            _ -> "コマンドは一つづつ使って下さい。"
    lookupDic k = fromMaybe "未知の単語です" $ lookup k dic

dummyDic :: [(Text, Text)]
dummyDic =
  [ ("hvguy8f", "hvguy8fはbvjgvuの別名"),
    ("bvjgvu", "石川県の野菜")
  ]

readMDDic :: FilePath -> IO [(Text, Text)]
readMDDic f = do
  c <- T.readFile f
  let (v, mk, d) = foldl build ("", Nothing, []) (T.lines c)
  pure $ maybe d (\k -> (T.strip k, v) : d) mk
  where
    build :: (Text, Maybe Text, [(Text, Text)]) -> Text -> (Text, Maybe Text, [(Text, Text)])
    build (v, Just k, acc) (T.stripPrefix "##" -> Just x) = ("", Just x, (T.strip k, v) : acc)
    build (_, _, acc) (T.stripPrefix "##" -> Just x) = ("", Just x, acc)
    build (v, k, acc) x = (v <> x, k, acc)
