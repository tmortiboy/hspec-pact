{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Lens (at, view, (&), (.~), (?~))
import           Data.Aeson (FromJSON, Value(..), object, (.=))
import           GHC.Generics (Generic)
import           Network.Wreq (Options, asJSON, defaults, getWith, header, responseBody)
import           Test.Hspec (hspec, it, shouldBe)

import           Test.Hspec.Pact

main :: IO ()
main =
  hspec $
    withPactMockService defaultPactConfig $ do

      generatePactBetween "my app" "your API" $ do

        it "Should call foo" $ do
          let request  = expectedRequest Get "/foo"
                         & erqHeaders . at "Accept" ?~ "application/json"
              response = expectedResponse
                         & ersHeaders . at "Content-Type" ?~ "application/json"
                         & ersBody ?~ object [ "result" .= String "from foo" ]
          setInteractionGiven "foo exists" request response $ \clientEnv -> do
            TestResult{..} <- getResultFrom clientEnv "/foo"
            result `shouldBe` "from foo"

        it "Should call bar" $ do
          let request  = expectedRequest Get "/bar"
                         & erqHeaders . at "Accept" ?~ "application/json"
              response = expectedResponse
                         & ersHeaders . at "Content-Type" ?~ "application/json"
                         & ersBody ?~ object [ "result" .= String "from bar" ]
          setInteractionGiven "bar exists" request response $ \clientEnv -> do
            TestResult{..} <- getResultFrom clientEnv "/bar"
            result `shouldBe` "from bar"

      generatePactBetween "my other app" "your other API" $

        it "Should call other" $ do
          let request  = expectedRequest Get "/other"
                         & erqHeaders . at "Accept" ?~ "application/json"
              response = expectedResponse
                         & ersHeaders . at "Content-Type" ?~ "application/json"
                         & ersBody ?~ object [ "result" .= String "from other" ]
          setInteractionGiven "other exists" request response $ \clientEnv -> do
            TestResult{..} <- getResultFrom clientEnv "/other"
            result `shouldBe` "from other"

getResultFrom :: ClientEnv -> String -> IO TestResult
getResultFrom (ClientEnv _ baseUrl _) uri = do
  response <- getWith options $ showBaseUrl baseUrl ++ uri
  view responseBody <$> asJSON response

options :: Options
options = defaults & header "Accept" .~ ["application/json"]

newtype TestResult = TestResult { result :: String } deriving (Show, Eq, Generic, FromJSON)
