{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module OpenAI.Type where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson
import qualified Data.Char as C


-- APIリクエストの型
data OpenAIRequest = OpenAIRequest
  { model :: Text
  , prompt :: Maybe Text
  , temperature :: Maybe Float
  , max_tokens :: Maybe Int
  , n :: Maybe Int
  , stop :: Maybe [Text]
  , top_p :: Maybe Float
  , presence_penalty :: Maybe Float
  , frequency_penalty :: Maybe Float
  , messages :: [Message]
  } deriving (Generic, Show)

instance ToJSON OpenAIRequest where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

data Message = Message
  { role :: Role
  , content :: Text
  } deriving (Generic, Show)
instance ToJSON Message
instance FromJSON Message

data Role = System | User | Assistant
  deriving (Generic, Show)

instance ToJSON Role where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = map C.toLower }

instance FromJSON Role where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = map C.toLower }


-- APIレスポンスの型
data OpenAIResponse = OpenAIResponse
  { id :: Text
  , object :: Text
  , created :: Int
  , model :: Text
  , usage :: UsageData
  , choices :: [ChoiceData]
  } deriving (Generic, Show)

instance FromJSON OpenAIResponse

data UsageData = UsageData
  { prompt_tokens :: Int
  , total_tokens :: Int
  } deriving (Generic, Show)

instance FromJSON UsageData

data ChoiceData = ChoiceData
  { message :: Message
  , finish_reason :: Text
  } deriving (Generic, Show)

instance FromJSON ChoiceData
