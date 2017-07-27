{-# LANGUAGE ViewPatterns #-}

module Walls.API.Files where

import qualified Data.Aeson as Aeson
import qualified Network.Wreq as Wreq

import           Control.Monad.Except
import           Data.Aeson.Lens
import           Servant.API
import           Servant.Server

import qualified Config.Config as Config

import           P
import           Walls.Utils.Servant

data XFile =
  XFile { name :: Text, size :: Integer
        } deriving (Show, Generic)

instance ToJSON XFile

type ReadAPI =
  "files" :> Header "Cookie" Text :> Get' [XFile]

type API =
  ReadAPI

imp :: Config.T -> IO (Server API)
imp config = do
  pure (_read config)

_read :: Config.T -> Maybe Text -> Handler [XFile]
_read config cookie = do
  secret <- fromSession config cookie
  files <- liftIO $ _filesFromSlack secret 0
  pure files

_filesFromSlack :: Secret -> Int -> IO [XFile]
_filesFromSlack secret page = do
  let opts =
        Wreq.defaults & param "token" .~ [view coerced secret]
                      & param "count" .~ ["1000"]
                      & param "page" .~ [show page & view packed]
  resp <- Wreq.getWith opts "https://slack.com/api/files.list"
  pure (resp ^.. Wreq.responseBody . key "files" . values & fmap _toXFile)
  where
    param = Wreq.param

_toXFile :: Aeson.Value -> XFile
_toXFile v = XFile (v ^. key "name" . _String) (fromMaybe 0 $ v ^? key "size" . _Integer)
