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
  XFile { id :: Text
        , name :: Text
        , size :: Integer
        , url :: Text
        , channels :: [Text]
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
  files <- liftIO $ _allFilesFromSlack secret
  pure files

_allFilesFromSlack :: Secret -> IO [XFile]
_allFilesFromSlack secret =
  loop 1
  where
    loop i = do
      files <- _filesFromSlack secret i
      case files of
        [] -> pure []
        _ -> do
          rest <- loop (i + 1)
          pure (_top 10 $ files ++ rest)

_top :: Int -> [XFile] -> [XFile]
_top n = take n . sortBy (comparing (Down . size))

_filesFromSlack :: Secret -> Int -> IO [XFile]
_filesFromSlack secret page = do
  let opts =
        Wreq.defaults & param "token" .~ [view coerced secret]
                      & param "count" .~ ["1000"]
                      & param "page" .~ [show page & view packed]
  resp <- Wreq.getWith opts "https://slack.com/api/files.list"
  print opts
  pure (resp ^.. Wreq.responseBody . key "files" . values & fmap _toXFile)
  where
    param = Wreq.param

_toXFile :: Aeson.Value -> XFile
_toXFile v =
  XFile (v ^. key "id" . _String)
        (v ^. key "name" . _String)
        (fromMaybe 0 $ v ^? key "size" . _Integer)
        (v ^. key "url_private" . _String)
        (v ^.. key "channels" . _Array . traverse & map (view _String))
