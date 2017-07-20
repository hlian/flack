{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Walls.Utils.Servant where

import           Servant.API

import qualified Config.Config as Config
import           P hiding (id)

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
  let days = view coerced maxAge :: Int
  let seconds = days * 24 * 60 * 60
  "id=" <> id <> "; HttpOnly; Max-Age=" <> view (to show . packed) seconds <> "; Path=" <> path

decodeSession :: Text -> Maybe Session
decodeSession x = error (show x)
