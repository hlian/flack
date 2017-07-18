module Walls.API where

import           Servant.Server

import qualified Config.Config as Config
import qualified Walls.API.OAuth as OAuth

import           P

type API =
  OAuth.API

apiProxy :: Proxy API
apiProxy = Proxy

apiImp :: Config.T -> IO (Server API)
apiImp config =
  OAuth.imp config
