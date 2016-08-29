module Walls.API where

import P
import Servant.Server
import Walls.Utils.Servant

data XPost =
  XPost { author :: !Text
        , body :: !Text
        }
deriveToJSON defaultOptions ''XPost

data XWall =
  XWall { name :: !Text
        , avatarURL :: !Text
        , posts :: ![XPost]
        }
deriveToJSON defaultOptions ''XWall

type API =
  Get' XWall

apiProxy :: Proxy API
apiProxy = Proxy

apiImp :: Server API
apiImp =
  return (XWall "@haoformayor" "http://i.imgur.com/NaijsSY.jpg" posts_)
  where
    posts_ =
      [ XPost "@friend" "hi friend"
      , XPost "@enemy" "hi enemy"
      ]
