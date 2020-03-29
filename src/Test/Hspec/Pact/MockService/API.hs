{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hspec.Pact.MockService.API
    ( API
    ) where

import           Control.Arrow (left)
import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.Typeable (Typeable)
import           Network.HTTP.Media ((//))
import           Servant

import           Test.Hspec.Pact.Types (Interactions, PactDetails)

type API =
  APIClearInteractions
  :<|> APIClearSession
  :<|> APISetInteractions
  :<|> APIVerifyInteractions
  :<|> APIGeneratePact

-- DELETE /interactions
type APIClearInteractions =
  "interactions"
  :> Header "X-Pact-Mock-Service" Bool
  :> Delete '[OctetStream] ByteString

-- DELETE /session
type APIClearSession =
  "session"
  :> Header "X-Pact-Mock-Service" Bool
  :> Delete '[OctetStream] ByteString

-- PUT /interactions
type APISetInteractions =
  "interactions"
  :> Header "X-Pact-Mock-Service" Bool
  :> ReqBody '[JSON] Interactions
  :> Put '[OctetStream] ByteString

-- GET /interactions/verification
type APIVerifyInteractions =
  "interactions"
  :> "verification"
  :> Header "X-Pact-Mock-Service" Bool
  :> Get '[PlainText'] Text

-- POST /pact
type APIGeneratePact =
  "pact"
  :> Header "X-Pact-Mock-Service" Bool
  :> ReqBody '[JSON] PactDetails
  :> Post '[JSON] Value

data PlainText' deriving Typeable

instance Accept PlainText' where
  contentType _ = "text" // "plain"

instance MimeRender PlainText' Text where
  mimeRender _ = fromStrict . encodeUtf8

instance MimeUnrender PlainText' Text where
  mimeUnrender _ = left show . decodeUtf8' . toStrict
