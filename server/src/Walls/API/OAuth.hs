module Walls.API.OAuth where

import           Control.Monad.Except
import           Servant.API
import           Servant.Server

import qualified Config.Config as Config
import qualified Slack.Slack as Slack

import           P hiding (Handler)
import           Walls.Utils.Servant

data XRedirect =
  XRedirect { clientID :: Text
            , clientSecret :: Text
            , code :: Text
            , redirectURI :: Text
            } deriving (Show, Generic)

data XDebug =
  XDebug { debug :: Text } deriving (Show, Generic)

instance ToJSON XRedirect

instance ToJSON XDebug

type RedirectAPI =
  "oauth" :> "redirect" :> QueryParam "code" Text :> QueryParam "state" Text :> Get' ()

type DebugAPI =
  "oauth" :> "debug" :> Header "Cookie" Text :> Get' XDebug

type API =
  RedirectAPI :<|> DebugAPI

imp :: Config.T -> IO (Server API)
imp config = do
  let auth = Slack.T { clientID = view Config.clientID config
                     , clientSecret = view (Config.clientSecret . coerced) config
                     , redirectURI = view Config.listen config <> "api/oauth/redirect"
                     , team = view Config.team config
                     }
  warn ("main: hit this: " <> show (Slack.authorizeURL auth))
  pure (_redirect config auth :<|> _debug)

_redirect :: Config.T -> Slack.T -> Maybe Text -> Maybe Text -> Handler ()
_redirect config auth codeM _ = do
  case codeM of
    Just code -> do
      tokenEither <- liftIO $ Slack.access auth code
      case tokenEither of
        Left e -> do
          throwError err400 { errBody = view lazy (utf8 # ("unable to grab access token: " <> e)) }
        Right token -> do
          let session = Session { token = view coerced token
                                , maxAge = Days 14
                                }
          let headers = [ ("Set-Cookie", utf8 # encodeSession config session)
                        , ("Location", utf8 # (view Config.listen config <> "api/oauth/debug"))
                        ]
          throwError err303 { errHeaders = headers }
    _ ->
      throwError err400 { errBody = "needed a code and a state" }

_debug :: Maybe Text -> Handler XDebug
_debug = \case
  Nothing ->
    throwError $ err400 { errBody = "unauthorized" }
  Just cookie ->
    pure (XDebug cookie)
