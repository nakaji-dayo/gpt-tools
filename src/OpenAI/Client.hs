{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Client where

import Data.Aeson (FromJSON (parseJSON), Options (constructorTagModifier, omitNothingFields), ToJSON (toJSON), defaultOptions, eitherDecode, encode, genericParseJSON, genericToJSON)
import Data.Char qualified as C
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseStatusCode, setRequestBodyLBS, setRequestHeader, setRequestMethod)
import OpenAI.Type

defaultGPT35 :: OpenAIRequest
defaultGPT35 =
  OpenAIRequest
    { model = "gpt-3.5-turbo",
      prompt = Nothing,
      temperature = Nothing,
      max_tokens = Nothing,
      n = Nothing,
      stop = Nothing,
      top_p = Nothing,
      presence_penalty = Nothing,
      frequency_penalty = Nothing,
      messages =
        []
    }

callAPI :: Text -> OpenAIRequest -> IO (Either String OpenAIResponse)
callAPI key requestOptions = do
  request <- parseRequest "https://api.openai.com/v1/chat/completions"
  let apiRequest =
        setRequestMethod "POST"
          . setRequestHeader "Content-Type" ["application/json"]
          . setRequestHeader "Authorization" ["Bearer " <> encodeUtf8 key]
          . setRequestBodyLBS (encode requestOptions)
          $ request
  manager <- newManager tlsManagerSettings
  response <- httpLbs apiRequest manager
  case getResponseStatusCode response of
    200 -> do
      pure $ eitherDecode (responseBody response)
    _ -> pure $ Left (show $ responseBody response)
