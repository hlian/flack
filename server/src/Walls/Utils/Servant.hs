{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Walls.Utils.Servant where

import qualified Data.Text as Text
import qualified Network.URI.Encode as URI

import           Control.Monad.Except
import           Servant.API
import           Servant.Server

import qualified CheapDB.CheapDB as CheapDB
import qualified Config.Config as Config
import           P

type Get' = Get '[JSON]
type Post' = Post '[JSON]
type Delete' = Delete '[JSON]
type ReqBody' = ReqBody '[JSON]

newtype Days =
  Days Int deriving (Show, Eq, Ord, Num)

data Session =
  Session { id :: Text
          , maxAge :: Days
          } deriving (Show, Eq, Ord)

encodeSession :: Config.T -> Session -> Text
encodeSession config Session{..} = do
  let path = view Config.path config
  "id=" <> URI.encodeText id <> "; path=" <> path <> ";domain=" <> view Config.addr config

decodeSession :: Maybe Text -> Maybe Session
decodeSession (fromMaybe "" -> t) = do
  id <- (Text.splitOn "=" t) ^? ix 1
  pure (Session { id = id, maxAge = Days 14 })

fromSession :: Config.T -> Maybe Text -> Handler Secret
fromSession config t = do
  let db = view Config.db config
  case decodeSession t of
    Just Session{..} -> do
      tokenM <- liftIO $ CheapDB.read db id
      case tokenM of
        Nothing ->
          throwError err401 { errBody = "stale cookie" }
        Just token ->
          pure token
    _ ->
      throwError err401 { errBody = "no cookie" }

fromSessionMaybe :: Config.T -> Maybe Text -> Handler (Maybe Secret)
fromSessionMaybe config t = do
  let db = view Config.db config
  case decodeSession t of
    Just Session{..} -> do
      liftIO $ CheapDB.read db id
    _ ->
      pure Nothing
