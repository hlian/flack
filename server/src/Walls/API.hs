module Walls.API where

import           Servant.API
import           Servant.Server

import qualified Config.Config as Config
import qualified Walls.API.OAuth as OAuth
import qualified Walls.API.Files as Files

import           P

type API =
  OAuth.API :<|> Files.API

apiProxy :: Proxy API
apiProxy = Proxy

apiImp :: Config.T -> IO (Server API)
apiImp config = do
  oauth <- OAuth.imp config
  files <- Files.imp config
  pure (oauth :<|> files)
