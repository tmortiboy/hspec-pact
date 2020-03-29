module Test.Hspec.Pact.Broker.Client
    ( brokerPublish
    ) where

import           Data.Aeson (Value)
import           Data.Proxy (Proxy(..))
import           Data.Version (Version)
import           Servant.Client (ClientM, client)

import           Test.Hspec.Pact.Broker.API (API)
import           Test.Hspec.Pact.Types (Name, Pact(..))

brokerPublish :: Version -> Pact -> ClientM Value
brokerPublish version pact@Pact{..} = brokerPublish' _pProvider _pConsumer version pact

brokerPublish' :: Name -> Name -> Version -> Pact -> ClientM Value
brokerPublish' =
  client (Proxy :: Proxy API)
