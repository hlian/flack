module Walls.API.OAuth where

import qualified Crypto.Hash as Crypto

import           Control.Monad.Except
import           Servant.API
import           Servant.Server

import qualified CheapDB.CheapDB as CheapDB
import qualified Config.Config as Config
import qualified Slack.Slack as Slack

import           P
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
  "oauth" :> "redirect" :> Header "Cookie" Text :> QueryParam "code" Text :> Post' ()

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
                     , redirectURI = Config.listen config <> "redirect.html"
                     , team = view Config.team config
                     }
  pure (_read config auth :<|> _redirect config auth :<|> _delete config)

_new :: Slack.T -> Handler Text
_new auth = do
  pure (Slack.authorizeURL auth)

_redirect :: Config.T -> Slack.T -> Maybe Text -> Maybe Text -> Handler ()
_redirect config auth (decodeSession -> sessionM) codeM = do
  case (sessionM, codeM) of
    (Just Session{..}, Just code) |
      _hash code == id -> throwError err303 { errHeaders = location }
    _ -> do
      code <- cleanM "missing ?code" codeM
      tokenEither <- liftIO $ Slack.access auth code
      token <- cleanE ("access problem: " <>) tokenEither
      let id = _hash code
      liftIO . void $ CheapDB.write db id token
      let session = Session { id = id , maxAge = Days 14 }
      let cookies = [("Set-Cookie", utf8 # encodeSession config session)]
      throwError err303 { errHeaders = cookies <> location }
  where
    db = view Config.db config
    location =
      [("Location", utf8 # (Config.listen config <> "api/oauth"))]
    cleanM :: ByteString -> Maybe a -> Handler a
    cleanM e = \case
      Just a -> pure a
      Nothing -> throwError err400 { errBody = view lazy e }
    cleanE :: (e -> Text) -> Either e a -> Handler a
    cleanE f = \case
      Right a -> pure a
      Left e -> throwError err400 { errBody = view (re utf8 . lazy) (f e) }

_read :: Config.T -> Slack.T -> Maybe Text -> Handler XRead
_read config auth cookie = do
  let sessionMaybe = decodeSession cookie
  secretMaybe <- fromSessionMaybe config cookie
  case (sessionMaybe, secretMaybe) of
    (Just Session{..}, Just _) ->
      pure (XRead (Just id) (Slack.authorizeURL auth))
    _ -> do
      pure (XRead Nothing (Slack.authorizeURL auth))

_delete :: Config.T -> Handler ()
_delete config = do
  let session = Session { id = "" , maxAge = Days 14 }
  let headers = [ ("Set-Cookie", utf8 # encodeSession config session)
                , ("Location", utf8 # (Config.listen config <> "api/oauth"))
                ]
  throwError err303 { errHeaders = headers }

_hash :: Text -> Text
_hash input =
  view packed $ show (Crypto.hash (utf8 # input) :: Crypto.Digest Crypto.MD5)
