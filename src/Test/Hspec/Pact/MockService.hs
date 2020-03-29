{-# LANGUAGE TemplateHaskell #-}

module Test.Hspec.Pact.MockService
    ( MockServiceConfig(..)
    , mscPactFolder
    , mscOSXDownloadURI
    , mscWindowsDownloadURI
    , mscLinuxDownloadURI
    , mscLinux64DownloadURI
    , mscDownloadMaxTimeout
    , mscServicePath
    , mscExePath
    , mscWindowsExePath
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
    , SpawnedServiceConfig(..)
    , spawnService
    , closeSpawnedService
    ) where

import qualified Codec.Archive.Tar as Tar hiding (unpack)
import qualified Codec.Archive.Tar.Unpack as Tar (unpack)
import           Codec.Archive.Zip (ZipOption(..))
import qualified Codec.Archive.Zip as Zip
import qualified Codec.Compression.GZip as GZip
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race, race_)
import           Control.Lens (makeLenses)
import           Control.Monad (unless)
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (catMaybes)
import           Data.Semigroup ((<>))
import           Data.Text (isInfixOf, pack)
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Conduit (httpLbs, parseRequest, responseBody)
import           Servant.Client (BaseUrl(..), ClientEnv(..), Scheme(..))
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath (replaceExtension, takeExtension, takeFileName, (</>))
import           System.Info (arch, os)
import           System.IO (Handle, hClose, hGetLine)
import           System.Process
                  (CreateProcess, ProcessHandle, StdStream(..), createProcess, proc, std_err,
                  terminateProcess)

data MockServiceConfig =
  MockServiceConfig
    { _mscPactFolder             :: FilePath
    , _mscOSXDownloadURI         :: String
    , _mscWindowsDownloadURI     :: String
    , _mscLinuxDownloadURI       :: String
    , _mscLinux64DownloadURI     :: String
    , _mscDownloadMaxTimeout     :: Int
    , _mscServicePath            :: FilePath
    , _mscExePath                :: FilePath
    , _mscWindowsExePath         :: FilePath
    , _mscStartServiceMaxTimeout :: Int
    , _mscPort                   :: Int
    , _mscHost                   :: String
    , _mscContractPath           :: FilePath
    , _mscWriteMode              :: Maybe PactFileWriteMode
    , _mscSpecificationVersion   :: Int
    , _mscLogPath                :: FilePath
    , _mscCors                   :: Maybe String
    , _mscSslMode                :: SSLMode }
  deriving (Show, Eq)

data PactFileWriteMode =
    Overwrite
  | Merge
  deriving (Eq)

instance Show PactFileWriteMode where
  show Overwrite = "overwrite"
  show Merge     = "merge"

data SSLMode =
    NoSSL
  | SelfSignedSSL
  | SSLCertKey FilePath FilePath
  deriving (Show, Eq)

makeLenses ''MockServiceConfig

defaultPactConfig :: MockServiceConfig
defaultPactConfig = do
  let baseDownloadURI = "https://github.com/pact-foundation/pact-mock_service/releases/download/v2.1.0/"
  MockServiceConfig
    { _mscPactFolder             = "pact"
    , _mscOSXDownloadURI         = baseDownloadURI <> "pact-mock-service-2.1.0-1-osx.tar.gz"
    , _mscWindowsDownloadURI     = baseDownloadURI <> "pact-mock-service-2.1.0-1-win32.zip"
    , _mscLinuxDownloadURI       = baseDownloadURI <> "pact-mock-service-2.1.0-1-linux-x86.tar.gz"
    , _mscLinux64DownloadURI     = baseDownloadURI <> "pact-mock-service-2.1.0-1-linux-x86_64.tar.gz"
    , _mscDownloadMaxTimeout     = 30000000
    , _mscServicePath            = "pact-mock-service"
    , _mscExePath                = "bin" </> "pact-mock-service"
    , _mscWindowsExePath         = "bin" </> "pact-mock-service.bat"
    , _mscStartServiceMaxTimeout = 10000000
    , _mscPort                   = 1234
    , _mscHost                   = "localhost"
    , _mscContractPath           = "pact-contracts"
    , _mscWriteMode              = Nothing
    , _mscSpecificationVersion   = 2
    , _mscLogPath                = "mock-service.logs"
    , _mscCors                   = Nothing
    , _mscSslMode                = NoSSL }

sslModeToScheme :: SSLMode -> Scheme
sslModeToScheme = \case
  NoSSL         -> Http
  SelfSignedSSL -> Https
  SSLCertKey{}  -> Https

configToArgs :: MockServiceConfig -> [String]
configToArgs MockServiceConfig{..} = catMaybes
  [ Just $ "--port " <> show _mscPort
  , Just $ "--host " <> _mscHost
  , Just $ "--pact-dir " <> _mscPactFolder </> _mscContractPath
  , ("--pact-file-write-mode " <>) . show <$> _mscWriteMode
  , Just $ "--pact-specification-version " <> show _mscSpecificationVersion
  , Just $ "--log " <> _mscPactFolder </> _mscServicePath </> _mscLogPath
  , ("--cors " <>) <$> _mscCors ]
  <> sslModeToArg _mscSslMode
  where
    sslModeToArg :: SSLMode -> [String]
    sslModeToArg = \case
      NoSSL               -> []
      SelfSignedSSL       -> ["--ssl"]
      SSLCertKey cert key -> ["--ssl", "--sslcert " <> cert, "--sslkey " <> key]

configToClientEnv :: MockServiceConfig -> Manager -> ClientEnv
configToClientEnv MockServiceConfig{..} manager = do
  let scheme = sslModeToScheme _mscSslMode
      mockServiceUrl = BaseUrl scheme _mscHost _mscPort mempty
  ClientEnv manager mockServiceUrl Nothing

getDownloadURI :: MockServiceConfig -> String
getDownloadURI MockServiceConfig{..} =
  case os of
    "mingw32"            -> _mscWindowsDownloadURI
    "darwin"             -> _mscOSXDownloadURI
    _ | arch == "x86_64" -> _mscLinux64DownloadURI
    _                    -> _mscLinuxDownloadURI

getExtractedPath :: MockServiceConfig -> FilePath
getExtractedPath config@MockServiceConfig{..} = do
  let downloadURI = getDownloadURI config
      extractedName = removeExtensions $ takeFileName downloadURI
  _mscPactFolder </> _mscServicePath </> extractedName
  where
    removeExtensions :: FilePath -> FilePath
    removeExtensions path =
      if takeExtension path `elem` [".tar", ".gz", ".tgz", ".zip"]
      then removeExtensions $ replaceExtension path ""
      else path

getFullExePath :: MockServiceConfig -> FilePath
getFullExePath config@MockServiceConfig{..} =
  getExtractedPath config </> case os of
     "mingw32" -> _mscWindowsExePath
     _         -> _mscExePath

mockerServiceCreateProcess :: MockServiceConfig -> CreateProcess
mockerServiceCreateProcess config = do
  let exePath = getFullExePath config
      args = configToArgs config
  (proc exePath args) { std_err = CreatePipe }

downloadMockService :: MockServiceConfig -> Manager -> IO ()
downloadMockService config@MockServiceConfig{..} manager = do
  let serviceFolder = _mscPactFolder </> _mscServicePath
  createDirectoryIfMissing True serviceFolder
  let exePath = getFullExePath config
  exeExists <- doesFileExist exePath
  unless exeExists $ race_ (threadDelay _mscDownloadMaxTimeout) (downloadAndExtractTo serviceFolder)
  where
    downloadAndExtractTo :: FilePath -> IO ()
    downloadAndExtractTo destination = do
      let downloadURI = getDownloadURI config
      request <- parseRequest downloadURI
      raw <- responseBody <$> httpLbs request manager
      case takeExtension downloadURI of
        ".zip" -> extractZipTo destination raw
        _      -> extractTarTo destination raw

    extractTarTo :: FilePath -> ByteString -> IO ()
    extractTarTo destination = Tar.unpack destination . Tar.read . GZip.decompress

    extractZipTo :: FilePath -> ByteString -> IO ()
    extractZipTo destination =
      Zip.extractFilesFromArchive [OptRecursive, OptDestination destination] . Zip.toArchive

data SpawnedServiceConfig =
  SpawnedServiceConfig
    { _sscClientEnv     :: ClientEnv
    , _sscProcessHandle :: ProcessHandle
    , _sccOutputHandle  :: Handle }

spawnService :: MockServiceConfig -> IO SpawnedServiceConfig
spawnService config@MockServiceConfig{..} = do
  manager <- newTlsManager

  downloadMockService config manager

  createProcess (mockerServiceCreateProcess config) >>= \case
    (_, _, Nothing, _sscProcessHandle) -> do
      terminateProcess _sscProcessHandle
      fail "Mock Service not started"
    (_, _, Just _sccOutputHandle, _sscProcessHandle) -> do
      let _sscClientEnv = configToClientEnv config manager
      started <- either id id <$> race (checkStarted _sccOutputHandle) timeOut
      if started
      then pure SpawnedServiceConfig{..}
      else do
        closeSpawnedService SpawnedServiceConfig{..}
        fail "Mock Service not started"
  where
    checkStarted :: Handle -> IO Bool
    checkStarted h = do
      outLine <- hGetLine h
      if "HTTPServer#start" `isInfixOf` pack outLine
      then pure True
      else checkStarted h

    timeOut :: IO Bool
    timeOut = False <$ threadDelay _mscStartServiceMaxTimeout

closeSpawnedService :: SpawnedServiceConfig -> IO ()
closeSpawnedService SpawnedServiceConfig{..} = do
  terminateProcess _sscProcessHandle
  hClose _sccOutputHandle
