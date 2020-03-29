{-# LANGUAGE TypeOperators #-}

module Test.Hspec.Pact.MockService.Client
    ( mockServiceClearInteractions
    , mockServiceClearSession
    , mockServiceSetInteractions
    , mockServiceVerifyInteractions
    , mockServiceGeneratePact
    ) where

import           Data.Aeson (Value)
import           Data.ByteString (ByteString)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Servant.API ((:<|>)(..))
import           Servant.Client (ClientM, client)

import           Test.Hspec.Pact.MockService.API (API)
import           Test.Hspec.Pact.Types (Interactions(..), PactDetails)

mockServiceClearInteractions :: ClientM ByteString
mockServiceClearInteractions = mockServiceClearInteractions' $ Just True

mockServiceClearSession :: ClientM ByteString
mockServiceClearSession = mockServiceClearSession' $ Just True

mockServiceSetInteractions :: Interactions -> ClientM ByteString
mockServiceSetInteractions = mockServiceSetInteractions' $ Just True

mockServiceVerifyInteractions :: ClientM Text
mockServiceVerifyInteractions = mockServiceVerifyInteractions' $ Just True

mockServiceGeneratePact :: PactDetails -> ClientM Value
mockServiceGeneratePact = mockServiceGeneratePact' $ Just True

mockServiceClearInteractions'  :: Maybe Bool -> ClientM ByteString
mockServiceClearSession'       :: Maybe Bool -> ClientM ByteString
mockServiceSetInteractions'    :: Maybe Bool -> Interactions -> ClientM ByteString
mockServiceVerifyInteractions' :: Maybe Bool -> ClientM Text
mockServiceGeneratePact'       :: Maybe Bool -> PactDetails -> ClientM Value
mockServiceClearInteractions'
  :<|> mockServiceClearSession'
  :<|> mockServiceSetInteractions'
  :<|> mockServiceVerifyInteractions'
  :<|> mockServiceGeneratePact' =
    client (Proxy :: Proxy API)
