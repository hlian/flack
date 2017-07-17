module Main where

import           Network.Wai.Handler.Warp (run)

import qualified Config.Config as Config
import qualified Slack.Slack as Slack
import           P
import           Servant.Server
import           Walls.API

main :: IO ()
main = do
  config <- Config.readConfig
  let w = Slack.T { clientID = view Config.clientID config
                  , clientSecret = view (Config.clientSecret . coerced) config
                  , redirectURI = view Config.listen config <> "/api/oauth/redirect"
                  , team = view Config.team config
                  }
  let server = apiImp w
  debug w
  finally (run 4000 (serve apiProxy server)) cleanup
  where
    cleanup =
      warn "main: and i had just begun to learn how to love"
    debug w = do
      warn ("main: hit this: " <> show (Slack.authorizeURL w))

