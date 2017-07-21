module Walls.API.OAuth where

import qualified Crypto.Random.Types as Crypto
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text

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

data XRead =
  XRead { id :: Maybe Text, slack :: Text } deriving (Show, Generic)

instance ToJSON XRedirect

instance ToJSON XRead

type RedirectAPI =
  "oauth" :> "redirect" :> QueryParam "code" Text :> QueryParam "state" Text :> Get' ()

type ReadAPI =
  "oauth" :> Header "Cookie" Text :> Get' XRead

type DeleteAPI =
  "oauth" :> "delete" :> Get' ()

type API =
  ReadAPI :<|> RedirectAPI :<|> DeleteAPI

imp :: Config.T -> IO (Server API)
imp config = do
  let auth = Slack.T { clientID = view Config.clientID config
                     , clientSecret = view Config.clientSecret config
                     , redirectURI = Config.listen config <> "api/oauth/redirect"
                     , team = view Config.team config
                     }
  pure (_read auth :<|> _redirect config auth :<|> _delete config)

_new :: Slack.T -> Handler Text
_new auth = do
  pure (Slack.authorizeURL auth)

_redirect :: Config.T -> Slack.T -> Maybe Text -> Maybe Text -> Handler ()
_redirect config auth codeM _ = do
  code <- cleanM "missing ?code" codeM
  tokenEither <- liftIO $ Slack.access auth code
  token <- cleanE ("access problem: " <>) tokenEither
  id <- liftIO _randomText
  liftIO . void $ CheapDB.write db id token
  let session = Session { id = id , maxAge = Days 14 }
  let headers = [ ("Set-Cookie", utf8 # encodeSession config session)
                , ("Location", utf8 # (Config.listen config <> "api/oauth"))
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

_read :: Slack.T -> Maybe Text -> Handler XRead
_read auth idM =
  pure (XRead (idM >>= parse) (Slack.authorizeURL auth))
  where
    parse t = case (Text.splitOn "=" t) ^? ix 1 of
      Just v -> pure v
      _ -> Nothing

_delete :: Config.T -> Handler ()
_delete config = do
  let session = Session { id = "" , maxAge = Days 14 }
  let headers = [ ("Set-Cookie", utf8 # encodeSession config session)
                , ("Location", utf8 # (Config.listen config <> "api/oauth"))
                ]
  throwError err303 { errHeaders = headers }

_randomText :: IO Text
_randomText = do
  (bytes :: ByteString) <- Crypto.getRandomBytes 16
  pure (Base16.encode bytes & view utf8)
