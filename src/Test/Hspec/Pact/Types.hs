{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Hspec.Pact.Types
    ( ExpectedMethod(..)
    , ExpectedRequest(..)
    , expectedRequest
    , erqMethod
    , erqPath
    , erqQuery
    , erqHeaders
    , erqBody
    , ExpectedResponse(..)
    , expectedResponse
    , expectedResponseStatus
    , ersStatus
    , ersHeaders
    , ersBody
    , ResponseStatus
    , Interaction(..)
    , Interactions(..)
    , PactDetails(..)
    , Name(..)
    , Pact(..)
    , Metadata(..)
    , PactSpecification(..)
    , Version
    ) where

import           Control.Applicative (optional)
import           Control.Lens (makeLenses)
import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Aeson.Casing (aesonDrop, aesonPrefix, camelCase)
import           Data.Map as Map
import           Data.Semigroup ((<>))
import           Data.Text
import           Data.Version (Version)
import           GHC.Generics
import           Servant.API (ToHttpApiData(..))

data ExpectedMethod =
    Get
  | Head
  | Post
  | Put
  | Patch
  | Delete
  | Trace
  | Connect
  deriving (Show, Eq, Generic)

instance ToJSON ExpectedMethod where
  toJSON = \case
    Get     -> String "get"
    Head    -> String "head"
    Post    -> String "post"
    Put     -> String "put"
    Patch   -> String "patch"
    Delete  -> String "delete"
    Trace   -> String "trace"
    Connect -> String "connect"

instance FromJSON ExpectedMethod where
  parseJSON = \case
    String "get"     -> pure Get
    String "head"    -> pure Head
    String "post"    -> pure Post
    String "put"     -> pure Put
    String "patch"   -> pure Patch
    String "delete"  -> pure Delete
    String "trace"   -> pure Trace
    String "connect" -> pure Connect
    _                -> mzero

type Path = Text
type ResponseStatus = Int

data ExpectedRequest =
  ExpectedRequest
    { _erqMethod  :: ExpectedMethod
    , _erqPath    :: Path
    , _erqQuery   :: Map Text Text
    , _erqHeaders :: Map Text Text
    , _erqBody    :: Maybe Value } --TODO for now just accepts JSON
  deriving (Show, Eq, Generic)

instance ToJSON ExpectedRequest where
   toJSON ExpectedRequest{..} = object $
     [ "method" .= _erqMethod
     , "path"   .= _erqPath ]
     <> addQuery
     <> addHeaders
     <> addBody
    where
      addHeaders = case _erqHeaders of
        headers | Map.size headers == 0 -> []
                | otherwise -> [ "headers" .= headers ]
      addQuery = case _erqQuery of
        query | Map.size query == 0 -> []
              | otherwise -> [ "query" .= query ]
      addBody = case _erqBody of
        Nothing   -> []
        Just body -> [ "body" .= body ]

instance FromJSON ExpectedRequest where
  parseJSON = withObject "ExpectedRequest" $ \o -> do
    _erqMethod  <- o .: "method"
    _erqPath    <- o .: "path"
    _erqQuery   <- o .:? "query" .!= Map.empty
    _erqHeaders <- o .:? "headers" .!= Map.empty
    _erqBody    <- optional (o .: "body")
    pure ExpectedRequest{..}

expectedRequest :: ExpectedMethod -> Path -> ExpectedRequest
expectedRequest method path = ExpectedRequest method path mempty mempty Nothing

makeLenses ''ExpectedRequest

data ExpectedResponse =
  ExpectedResponse
    { _ersStatus  :: ResponseStatus
    , _ersHeaders :: Map Text Text
    , _ersBody    :: Maybe Value } --TODO for now just accepts JSON
  deriving (Show, Eq, Generic)

instance ToJSON ExpectedResponse where
  toJSON ExpectedResponse{..} = object $
     [ "status" .= _ersStatus ]
     <> addHeaders
     <> addBody
    where
      addHeaders = case _ersHeaders of
        headers | Map.size headers == 0 -> []
                | otherwise -> [ "headers" .= headers ]
      addBody = case _ersBody of
        Nothing   -> []
        Just body -> [ "body" .= body ]

instance FromJSON ExpectedResponse where
  parseJSON = withObject "ExpectedResponse" $ \o -> do
    _ersStatus  <- o .: "status"
    _ersHeaders <- o .:? "headers" .!= Map.empty
    _ersBody    <- optional (o .: "body")
    pure ExpectedResponse{..}

expectedResponse :: ExpectedResponse
expectedResponse = ExpectedResponse 200 mempty Nothing

expectedResponseStatus :: ResponseStatus -> ExpectedResponse
expectedResponseStatus status = ExpectedResponse status mempty Nothing

makeLenses ''ExpectedResponse

data Interaction =
  Interaction
    { _iDescription   :: Text
    , _iProviderState :: Text
    , _iRequest       :: ExpectedRequest
    , _iResponse      :: ExpectedResponse }
  deriving (Show, Eq, Generic)

instance ToJSON Interaction where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Interaction where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

newtype Interactions =
  Interactions
    { _interactions :: [Interaction] }
  deriving (Show, Eq, Generic)

instance ToJSON Interactions where
   toJSON = genericToJSON $ aesonDrop 1 camelCase

instance FromJSON Interactions where
   parseJSON = genericParseJSON $ aesonDrop 1 camelCase

data PactDetails =
  PactDetails
    { _pdConsumer :: Name
    , _pdProvider :: Name }
  deriving (Show, Eq, Generic)

instance ToJSON PactDetails where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON PactDetails where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

newtype Name =
  Name
    { _name :: Text }
  deriving (Show, Eq, Generic)

instance ToJSON Name where
   toJSON = genericToJSON $ aesonDrop 1 camelCase

instance FromJSON Name where
   parseJSON = genericParseJSON $ aesonDrop 1 camelCase

instance ToHttpApiData Name where
  toUrlPiece = toUrlPiece . _name

data Pact =
  Pact
    { _pConsumer     :: Name
    , _pProvider     :: Name
    , _pInteractions :: [Interaction]
    , _pMetadata     :: Metadata }
  deriving (Show, Eq, Generic)

instance ToJSON Pact where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Pact where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

newtype Metadata =
  Metadata
    { _mPactSpecification :: PactSpecification }
  deriving (Show, Eq, Generic)

instance ToJSON Metadata where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Metadata where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

newtype PactSpecification =
  PactSpecification
    { _psVersion :: Version }
  deriving (Show, Eq, Generic)

instance ToJSON PactSpecification where
   toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON PactSpecification where
   parseJSON = genericParseJSON $ aesonPrefix camelCase
