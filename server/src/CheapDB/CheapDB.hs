module CheapDB.CheapDB where

import           Data.Map (Map)
import qualified Data.Map.Strict as Map

import           Control.Concurrent.STM.TMVar
import           P

newtype T k v = T (TMVar (Map k v))

new :: IO (T k v)
new =
  T <$> newTMVarIO Map.empty

read :: Ord k => T k v -> k -> IO (Maybe v)
read (T db) key = do
  inner <- atomically (readTMVar db)
  pure (inner ^. at key)

write :: Ord k => T k v -> k -> v -> IO ()
write (T db) key value =
  atomically $ do
    old <- readTMVar db
    _ <- swapTMVar db (old & at key .~ Just value)
    pure ()
