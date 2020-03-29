{-# LANGUAGE TypeApplications #-}

module Broker.Configuration
    ( BrokerConfig(..)
    , loadBrokerConfig
    ) where

import           Data.List (uncons)
import           Data.Semigroup ((<>))
import           Data.Version (Version, parseVersion)
import           Options.Applicative
import           System.FilePath ((</>))
import           Text.ParserCombinators.ReadP (readP_to_S)

import           Test.Hspec.Pact (BaseUrl(..), MockServiceConfig(..), Scheme(..), defaultPactConfig)

data BrokerConfig =
  BrokerConfig
    { _bcBrokerBaseUrl :: BaseUrl
    , _bcContractsPath :: FilePath
    , _bcVersion       :: Version }
  deriving (Show, Eq)

loadBrokerConfig :: IO BrokerConfig
loadBrokerConfig =
  execParser $
    info (helper <*> brokerConfigParser)
         (fullDesc <> header "PACT Broker publsher")

brokerConfigParser :: Parser BrokerConfig
brokerConfigParser =
  BrokerConfig
  <$> brokerBaseUrlParser
  <*> option str
    ( long "contracts"
      <> short 'c'
      <> metavar "PATH"
      <> help "The path where the PACT contracts are located."
      <> value defaultContractPath
      <> showDefault )
  <*> option readVersion
    ( long "version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "The version of the PACT contract to publish."
      <> showDefault )
  where
    MockServiceConfig{..} = defaultPactConfig
    defaultContractPath = _mscPactFolder </> _mscContractPath

brokerBaseUrlParser :: Parser BaseUrl
brokerBaseUrlParser =
  BaseUrl
  <$> option readUrlScheme
    ( long "broker-scheme"
      <> short 's'
      <> metavar "SCHEME"
      <> value Http
      <> help "The URL scheme of the PACT Broker, either http or https."
      <> showDefault )
  <*> option str
    ( long "broker-host"
      <> short 'h'
      <> metavar "HOSTNAME"
      <> help "The hostname of the PACT Broker."
      <> showDefault )
  <*> option auto --Int
    ( long "broker-port"
      <> short 'p'
      <> metavar "PORT"
      <> value 80
      <> help "The port of the PACT Broker."
      <> showDefault )
  <*> pure mempty

-- No Read instance for Scheme
readUrlScheme :: ReadM Scheme
readUrlScheme = str @String >>= \case
  "http"  -> pure Http
  "https" -> pure Https
  _       -> fail "Invalid url scheme, expected http or https"

readVersion :: ReadM Version
readVersion = findVersion . readP_to_S parseVersion =<< str
  where
    findVersion = maybe (fail "Invalid version.") (pure . fst) . uncons . fmap fst . reverse
