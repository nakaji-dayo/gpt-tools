{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module ChatM where
import OpenAI.Type
import OpenAI.Client
import GHC.Generics (Generic)
import Data.Text (Text, pack)
import Control.Monad.Trans.State (StateT, runState, execState, get, execStateT, put, gets, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Lens.Micro ((^.), (^?), ix)
import Control.Monad.Trans.Class (lift)
import Lens.Micro.Extras (view)
import Util (pprintChat)
import Data.ByteString.Lazy (fromChunks)


data ChatContext = ChatContext
  { apiKey :: Text
  , messages :: [Message]
  } deriving (Generic, Show)

type Chat = StateT ChatContext IO

-- runChat :: String -> Chat a -> IO a
runChat :: MonadIO m => String -> StateT ChatContext m a -> m a
runChat key f = evalStateT f (ChatContext (pack key) [])

msgsM :: [Message] -> Chat Message
msgsM msgs = do
  ctx <- get
  let msgs' = (ctx ^. #messages) ++ msgs
  res <- lift $ callAPI (ctx ^. #apiKey) $ defaultGPT35 {messages = msgs'}
  case res of
    Right x -> do
      case x ^? #choices . ix 0 . #message . #content of
        Just c -> do
          put $ ctx { messages = msgs' ++ [Message Assistant c] }
          pure (Message Assistant c)
        _ -> error "exit"
    Left e -> error e


msgM  :: Role -> Text -> Chat Message
msgM role txt = msgsM [Message role txt]

userM :: Text -> Chat Message
userM =  msgM User

systemM :: Text -> Chat Message
systemM =  msgM User

ppMagsM :: Chat ()
ppMagsM = do
  msgs <- gets (view #messages)
  lift $ pprintChat msgs

lastM :: Chat Message
lastM = last <$> gets (view #messages)
