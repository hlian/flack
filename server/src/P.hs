module P (
  module X
  , Text
  , ByteString
  , Aeson.typeMismatch
  , (.:=)
  , say
  , warn
  , reasonableAesonOptions) where

import           BasePrelude as X hiding ((&)
                                         , lazy
                                         , index
                                         , uncons
                                         , bracket
                                         , finally)
import           Control.Exception.Lifted as X (bracket, finally)
import           Control.Lens as X
import           Data.Aeson as X hiding ((.=))
import           Data.Aeson.TH as X
import           Data.ByteString (ByteString)
import           Data.String.Interpolate as X
import           Data.Text (Text)
import           Data.Text.Strict.Lens as X

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

import           System.Console.ANSI

(.:=) :: Aeson.ToJSON a => Text -> a -> Aeson.Pair
(.:=) = (Aeson..=)

reasonableAesonOptions :: Text -> Options
reasonableAesonOptions prefix =
  defaultOptions {
    fieldLabelModifier =
        view unpacked
      . Text.replace "iD" "id"
      . Text.replace "uRL" "url"
      . (ix 0 %~ toLower)
      . Text.drop (Text.length prefix)
      . view packed
  }

say :: String -> IO ()
say = putStrLn

warn :: String -> IO ()
warn s = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn s
  setSGR [Reset]
