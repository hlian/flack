module Config.Config where

import qualified CheapDB.CheapDB as CheapDB
import           P

data T =
  T { addr𝓕 :: Text
    , port𝓕 :: Int
    , path𝓕 :: Text
    , clientID𝓕 :: Text
    , clientSecret𝓕 :: Secret
    , team𝓕 :: Text
    , db𝓕 :: CheapDB.T Text Secret
    }

readConfig :: IO T
readConfig = do
  addr𝓕 <- _grab "addr" `orelse` "missing addr"
  port𝓕 <- _grab "port" `orelse` "missing port" & fmap (read . view unpacked)
  path𝓕 <- _grab "path" `orelse` "missing path"
  clientID𝓕 <- _grab "clientid" `orelse` "missing client id"
  clientSecret𝓕 <- _grab "clientsecret" `orelse` "missing client secret" & fmap Secret
  team𝓕 <- _grab "team" `orelse` "missing team"
  db𝓕 <- CheapDB.new
  pure T{..}
  where
    orelse :: IO (Maybe Text) -> String -> IO Text
    orelse io message = do
      io >>= \case
        Just t -> pure t
        Nothing -> error message

clientID :: Lens' T Text
clientID =
  lens (\T{..} -> clientID𝓕) (\t new -> t { clientID𝓕 = new })

clientSecret :: Lens' T Secret
clientSecret =
  lens (\T{..} -> clientSecret𝓕) (\t new -> t { clientSecret𝓕 = new })

addr :: Lens' T Text
addr =
  lens (\T{..} -> addr𝓕) (\t new -> t { addr𝓕 = new })

port :: Lens' T Int
port =
  lens (\T{..} -> port𝓕) (\t new -> t { port𝓕 = new })

path :: Lens' T Text
path =
  lens (\T{..} -> path𝓕) (\t new -> t { path𝓕 = new })

listen :: T -> Text
listen config =
  case view port config of
    80 -> "http://" <> view addr config <> view path config
    _ -> "http://" <> view addr config <> ":" <> view packed (show (view port config)) <> view path config

team :: Lens' T Text
team =
  lens (\T{..} -> team𝓕) (\t new -> t { team𝓕 = new })

db :: Lens' T (CheapDB.T Text Secret)
db =
  lens (\T{..} -> db𝓕) (\t new -> t { db𝓕 = new })

_grab :: String -> IO (Maybe Text)
_grab = (fmap . fmap) (view packed) . lookupEnv . ("flack" <>)
