## Hspec Pact

Integrates a Pact testing DSL into Hspec.

This still uses the ruby pact mock server, which is downloaded and started automatically, and can be configured via configuration passed to `withPactMockService`.

## Usage

```
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
            result <- getResultFrom clientEnv "/foo"
            result `shouldBe` TestResult "from foo"
```
See the tests for full code example.

`setInteractionGiven` takes a function which is passed a `ClientEnv`. This allows you to access the configured server details inside the tests, and you can pass this directly to Servant clients.

## Current Shortfalls

- Should be converted to assume mock service is being run in docker.
- Not yet tested on Windows or Linux (although the code has been written)
- Not much testing apart from integration - come on, this was a hackathon project :)
- No haddock comments yet.
- The request and response bodies can currently only accept JSON
- The tar archive extractor currently has an issue where it doesn't correctly set the executable bit, and so I have had to create a separate version of Tar.unpack which does. https://github.com/haskell/tar/issues/25
