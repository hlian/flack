module P (
  module X
  , Text
  , ByteString
  , Secret(..)
  , Aeson.typeMismatch
  , (.:=)
  , say
  , warn
  , reasonableAesonOptions) where

import           BasePrelude as X hiding ((&)
                                         , lazy
                                         , id
                                         , index
                                         , loop
                                         , uncons
                                         , bracket
                                         , finally
                                         , Handler)
import           Control.Exception.Lifted as X (bracket, finally)
import           Control.Lens as X
import           Data.Aeson as X hiding ((.=))
import           Data.Aeson.TH as X
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Strict.Lens as X

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

import           System.Console.ANSI

newtype Secret = Secret Text

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
