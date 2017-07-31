module Main where

import           Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.RequestLogger as Wai

import           Servant.Server

import qualified Config.Config as Config
import           P
import           Walls.API

main :: IO ()
main = do
  config <- Config.readConfig
  server <- apiImp config
  warn "walls: running on :4000; use nginx to reverse-proxy to me ._."
  finally (run 4000 (middleware $ serve apiProxy server)) cleanup
  where
    cleanup =
      warn "walls: and i had just begun to learn how to love"
    middleware =
      Wai.logStdoutDev
