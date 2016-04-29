module Main where

import Network.Wai.Handler.Warp (run)

import P
import Servant.Server
import Walls.API

port :: Int
port = 4000

main :: IO ()
main = do
  warn [i|main: running on port #{port}|]
  finally (run port (serve apiProxy apiImp)) cleanup
  where
    cleanup =
      warn [i|main: and i had just begun to learn how to love|]
