module Walls.API where

import           Servant.API
import           Servant.Server

import qualified Slack.Slack as Slack
import qualified Walls.API.OAuth as OAuth

import           P
import           Walls.Utils.Servant

data XPost =
  XPost { author :: !Text
        , body :: !Text
        } deriving (Show, Generic)

instance ToJSON XPost

data XWall =
  XWall { name :: !Text
        , avatarURL :: !Text
        , posts :: ![XPost]
        } deriving (Show, Generic)

instance ToJSON XWall

type API =
  Get' XWall :<|> OAuth.API

apiProxy :: Proxy API
apiProxy = Proxy

apiImp :: Slack.T -> Server API
apiImp authWorld =
  return (XWall "@haoformayor" "http://i.imgur.com/NaijsSY.jpg" posts_) :<|> OAuth.imp authWorld
  where
    posts_ =
      [ XPost "@friend" "hi friend"
      , XPost "@enemy" "hi enemy"
      ]
