{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hspec.Pact.Broker.API
    ( API
    ) where

import           Data.Aeson (Value)
import           Data.Version (Version)
import           Servant ((:>), Capture, JSON, Put, ReqBody)

import           Test.Hspec.Pact.Types (Name, Pact)

type API = APIPublishPact

-- PUT /pacts/provider/{provider}/consumer/{consumer}/version/{version}
type APIPublishPact =
  "pacts"
  :> "provider"
  :> Capture "provider" Name
  :> "consumer"
  :> Capture "consumer" Name
  :> "version"
  :> Capture "version" Version
  :> ReqBody '[JSON] Pact
  :> Put '[JSON] Value
