import           Control.Monad ((<=<))
import           Control.Monad.Catch (throwM)
import           Data.Aeson (eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as ByteString
import           Data.ByteString.UTF8 (toString)
import           Data.Semigroup ((<>))
import           Data.Version (Version, showVersion)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Directory (listDirectory)
import           System.FilePath (takeExtension, (</>))

import           Broker.Configuration (BrokerConfig(..), loadBrokerConfig)
import           Test.Hspec.Pact (ClientEnv(..), ClientM, Pact, brokerPublish, runClientM)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  BrokerConfig{..} <- loadBrokerConfig
  let clientEnv = ClientEnv manager _bcBrokerBaseUrl Nothing
  contractPaths <- getContractFilePaths _bcContractsPath
  publishPact clientEnv _bcVersion `mapM_` contractPaths

getContractFilePaths :: FilePath -> IO [FilePath]
getContractFilePaths contractsPath =
  fmap appendPath . filterJsonFiles <$> listDirectory contractsPath
  where
    filterJsonFiles = filter $ (".json" ==) . takeExtension
    appendPath fileName = contractsPath </> fileName

publishPact :: ClientEnv -> Version -> FilePath -> IO ()
publishPact clientEnv version filePath = do
  pact <- tryDecodeContract filePath
  putStrLn $ "Uploading contract " <> filePath <> " version " <> showVersion version
  result <- runBroker clientEnv $ brokerPublish version pact
  putStrLn . toString . toStrict $ encodePretty result
  where
    tryDecodeContract :: FilePath -> IO Pact
    tryDecodeContract = either fail pure . eitherDecode <=< ByteString.readFile

runBroker :: ClientEnv -> ClientM a -> IO a
runBroker clientEnv = either throwM pure <=< flip runClientM clientEnv
