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

fromSession :: Config.T -> Maybe Text -> Handler Secret
fromSession config (fromMaybe mempty -> t) = do
  let db = view Config.db config
  case (Text.splitOn "=" t) ^? ix 1 of
    Just id -> do
      tokenM <- liftIO $ CheapDB.read db id
      case tokenM of
        Nothing ->
          throwError err401 { errBody = "stale cookie" }
        Just token ->
          pure token
    _ ->
      throwError err401 { errBody = "no cookie" }

fromSessionMaybe :: Config.T -> Maybe Text -> Handler (Maybe Secret)
fromSessionMaybe config (fromMaybe mempty -> t) = do
  let db = view Config.db config
  case (Text.splitOn "=" t) ^? ix 1 of
    Just id -> do
      liftIO $ CheapDB.read db id
    _ ->
      pure Nothing
