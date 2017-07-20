module Walls.API.OAuth where

import qualified Crypto.Random.Types as Crypto
import qualified Data.ByteString.Base64 as Base64

import           Control.Monad.Except
import           Servant.API
import           Servant.Server

import qualified CheapDB.CheapDB as CheapDB
import qualified Config.Config as Config
import qualified Slack.Slack as Slack

import           P hiding (Handler, id)
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
                     , clientSecret = view Config.clientSecret config
                     , redirectURI = view Config.listen config <> "api/oauth/redirect"
                     , team = view Config.team config
                     }
  warn ("main: hit this: " <> show (Slack.authorizeURL auth))
  pure (_redirect config auth :<|> _debug)

_redirect :: Config.T -> Slack.T -> Maybe Text -> Maybe Text -> Handler ()
_redirect config auth codeM _ = do
  code <- cleanM "missing ?code" codeM
  tokenEither <- liftIO $ Slack.access auth code
  token <- cleanE ("access problem: " <>) tokenEither
  id <- liftIO _randomText
  liftIO . void $ CheapDB.write db id token
  let session = Session { id = id , maxAge = Days 14 }
  let headers = [ ("Set-Cookie", utf8 # encodeSession config session)
                , ("Location", utf8 # (view Config.listen config <> "api/oauth/debug"))
                ]
  throwError err303 { errHeaders = headers }
  where
    db = view Config.db config
    cleanM :: ByteString -> Maybe a -> Handler a
    cleanM e = \case
      Just a -> pure a
      Nothing -> throwError err400 { errBody = view lazy e }
    cleanE :: (e -> Text) -> Either e a -> Handler a
    cleanE f = \case
      Right a -> pure a
      Left e -> throwError err400 { errBody = view (re utf8 . lazy) (f e) }

_debug :: Maybe Text -> Handler XDebug
_debug = \case
  Nothing ->
    throwError $ err400 { errBody = "unauthorized" }
  Just cookie ->
    pure (XDebug cookie)

_randomText :: IO Text
_randomText = do
  (bytes :: ByteString) <- Crypto.getRandomBytes 128
  pure (Base64.encode bytes & view utf8)
