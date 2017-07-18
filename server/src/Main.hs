module Main where

import           Network.Wai.Handler.Warp (run)

import           Servant.Server

import qualified Config.Config as Config
import           P
import           Walls.API

main :: IO ()
main = do
  config <- Config.readConfig
  server <- apiImp config
  finally (run 4000 (serve apiProxy server)) cleanup
  where
    cleanup =
      warn "main: and i had just begun to learn how to love"
