module Slack.Slack where

import qualified Data.Text as Text
import qualified Network.URI.Encode as URI
import qualified Network.Wreq as Wreq

import           Data.Aeson.Lens
import           P

-- Unshowable, unencodable
newtype Secret = Secret Text

data T =
  T { clientID :: Text
    , clientSecret:: Secret
    , redirectURI :: Text
    , team :: Text
    }

authorizeURL :: T -> Text
authorizeURL T{..} =
  "https://slack.com/oauth/authorize?" <> _encode opts
  where
    opts = [ ("client_id", clientID)
           , ("scope", "users:read files:read files:write:user") -- stolen from Deletron
           , ("redirect_uri", redirectURI)
           , ("team", team)
           ]

access :: T -> Text -> IO ()
access T{..} code = do
  let opts =
        Wreq.defaults & param "client_id" .~ [clientID]
                      & param "client_secret" .~ [view coerced clientSecret :: Text]
                      & param "code" .~ [code]
                      & param "redirect_uri" .~ [redirectURI]
  resp <- Wreq.getWith opts "https://slack.com/api/oauth.access"
  print (resp ^. Wreq.responseBody . _Object)
  where
    param = Wreq.param

_encode :: [(Text, Text)] -> Text
_encode =
  Text.intercalate "&" . map (\(k, v) -> k <> "=" <> URI.encodeText v)
