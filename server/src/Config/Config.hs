module Config.Config where

import qualified Data.Text as Text
import           P

newtype Secret = Secret Text

data T =
  T { listen𝓕 :: Text
    , clientID𝓕 :: Text
    , clientSecret𝓕 :: Secret
    , team𝓕 :: Text
    }

readConfig :: IO T
readConfig = do
  listen𝓕 <- _grab "listen" `orelse` "missing listen"
  clientID𝓕 <- _grab "clientid" `orelse` "missing client id"
  clientSecret𝓕 <- _grab "clientsecret" `orelse` "missing client secret" & fmap Secret
  team𝓕 <- _grab "team" `orelse` "missing team"
  pure T{..}
  where
    orelse :: IO (Maybe Text) -> String -> IO Text
    orelse io message = do
      io >>= \case
        Just t -> pure t
        Nothing -> error message

port :: Lens' T Int
port =
  lens (\T{..} -> case Text.splitOn ":" listen𝓕 of
            [_, _, port_] -> read (view unpacked port_)
            _ -> error ("invalid listen: " <> show listen𝓕))
        (\t@T{..} newPort -> case Text.splitOn ":" listen𝓕 of
            [proto, name, _] ->
              t { listen𝓕 = proto <> ":" <> name <> ":" <> (view packed . show) newPort }
            _ -> error "invalid listen")

clientID :: Lens' T Text
clientID =
  lens (\T{..} -> clientID𝓕) (\t new -> t { clientID𝓕 = new })

clientSecret :: Lens' T Secret
clientSecret =
  lens (\T{..} -> clientSecret𝓕) (\t new -> t { clientSecret𝓕 = new })

listen :: Lens' T Text
listen =
  lens (\T{..} -> listen𝓕) (\t new -> t { listen𝓕 = new })

team :: Lens' T Text
team =
  lens (\T{..} -> team𝓕) (\t new -> t { team𝓕 = new })

_grab :: String -> IO (Maybe Text)
_grab = (fmap . fmap) (view packed) . lookupEnv . ("flack" <>)
