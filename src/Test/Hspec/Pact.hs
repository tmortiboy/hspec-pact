module Test.Hspec.Pact
    ( withPactMockService
    , MockServiceConfig(..)
    , mscPactFolder
    , mscOSXDownloadURI
    , mscWindowsDownloadURI
    , mscLinuxDownloadURI
    , mscLinux64DownloadURI
    , mscDownloadMaxTimeout
    , mscServicePath
    , mscExePath
    , mscStartServiceMaxTimeout
    , mscPort
    , mscHost
    , mscContractPath
    , mscWriteMode
    , mscSpecificationVersion
    , mscLogPath
    , mscCors
    , mscSslMode
    , PactFileWriteMode(..)
    , SSLMode(..)
    , defaultPactConfig
    , Consumer
    , Provider
    , generatePactBetween
    , setInteractionGiven
    , setInteractionsGiven
    , ExpectedMethod(..)
    , ExpectedRequest(..)
    , expectedRequest
    , erqMethod
    , erqPath
    , erqHeaders
    , erqBody
    , ExpectedResponse(..)
    , expectedResponse
    , expectedResponseStatus
    , ersStatus
    , ersHeaders
    , ersBody
    , ResponseStatus
    , ClientEnv(..)
    , ClientM
    , BaseUrl(..)
    , showBaseUrl
    , Scheme(..)
    , brokerPublish
    , Pact(..)
    , runClientM
    ) where

import           Control.Monad (void, (<=<))
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.UTF8 (toString)
import           Data.Foldable (toList)
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack, unpack)
import           Network.HTTP.Types.Header (hContentType)
import           Servant.Client
                  (BaseUrl(..), ClientEnv(..), ClientM, Scheme(..), ServantError(..), responseBody,
                  responseHeaders, runClientM, showBaseUrl)
import           Test.Hspec
                  (Expectation, Spec, SpecWith, after, afterAll, beforeAll, describe,
                  expectationFailure)

import           Test.Hspec.Pact.Broker.Client
import           Test.Hspec.Pact.MockService
import           Test.Hspec.Pact.MockService.Client
import           Test.Hspec.Pact.Types

withPactMockService :: MockServiceConfig -> SpecWith SpawnedServiceConfig -> Spec
withPactMockService config = beforeAll (spawnService config) . afterAll closeSpawnedService

type Consumer = Text
type Provider = Text

generatePactBetween ::
  Consumer -> Provider -> SpecWith SpawnedServiceConfig -> SpecWith SpawnedServiceConfig
generatePactBetween consumer provider spec = do
  let name = unpack $ "Pact tests between " <> consumer <> " and " <> provider
  afterAll (generatePact consumer provider) $
    after verifyInteractions $
      describe name spec

setInteractionGiven ::
  Text
  -> ExpectedRequest
  -> ExpectedResponse
  -> (ClientEnv -> Expectation)
  -> SpawnedServiceConfig
  -> Expectation
setInteractionGiven state request response = setInteractionsGiven state [(request, response)]

setInteractionsGiven ::
  Text
  -> [(ExpectedRequest, ExpectedResponse)]
  -> (ClientEnv -> Expectation)
  -> SpawnedServiceConfig
  -> Expectation
setInteractionsGiven state requestResponses action SpawnedServiceConfig{..} = do
  runClient _sscClientEnv $
    mockServiceSetInteractions . Interactions $
      uncurry requestResponseToInteraction <$> requestResponses
  action _sscClientEnv
  where
    requestResponseToInteraction request@ExpectedRequest{..} response =
      Interaction
        { _iDescription   = pack (show _erqMethod) <> " " <> _erqPath
        , _iProviderState = state
        , _iRequest       = request
        , _iResponse      = response }

verifyInteractions :: SpawnedServiceConfig -> Expectation
verifyInteractions SpawnedServiceConfig{..} =
  runClient _sscClientEnv mockServiceVerifyInteractions

generatePact :: Consumer -> Provider -> SpawnedServiceConfig -> Expectation
generatePact consumer provider SpawnedServiceConfig{..} = do
  let pactDetails = PactDetails (Name consumer) (Name provider)
  runClient _sscClientEnv $ mockServiceGeneratePact pactDetails
  runClient _sscClientEnv mockServiceClearSession

runClient :: ClientEnv -> ClientM a -> Expectation
runClient c = either servantErrorToFailure (void . pure) <=< flip runClientM c

servantErrorToFailure :: ServantError -> Expectation
servantErrorToFailure = \case
  FailureResponse response          -> expectationFailure . toString . toStrict . responseBody $ response
  DecodeFailure err _               -> expectationFailure . unpack $ err
  UnsupportedContentType media _    -> expectationFailure $
                                        "Unsupported content type: " <> show media
  InvalidContentTypeHeader response -> expectationFailure $
                                        "Invalid content type header: " <>
                                        (show . lookup hContentType . toList . responseHeaders $ response)
  ConnectionError err               -> expectationFailure $ "Connection error: " <> unpack err
