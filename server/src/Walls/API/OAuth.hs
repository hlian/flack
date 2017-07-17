module Walls.API.OAuth where

import           Control.Monad.Except
import           Servant.API
import           Servant.Server

import qualified Slack.Slack as Slack

import           P hiding (Handler)
import           Walls.Utils.Servant

data XRedirect =
  XRedirect { clientID :: Text
            , clientSecret :: Text
            , code :: Text
            , redirectURI :: Text
            } deriving (Show, Generic)

instance ToJSON XRedirect

type API =
  "oauth" :> "redirect" :> QueryParam "code" Text :> QueryParam "state" Text :> Get' ()

imp :: Slack.T -> Server API
imp authWorld =
  _redirect authWorld

_redirect :: Slack.T -> Maybe Text -> Maybe Text -> Handler ()
_redirect authWorld codeM _ = do
  liftIO $ print codeM
  case codeM of
    Just code -> do
      liftIO $ Slack.access authWorld code
    _ ->
      throwError $ err400 { errBody = "needed a code and a state" }
