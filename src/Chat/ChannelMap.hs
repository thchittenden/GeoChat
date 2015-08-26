module Chat.ChannelMap where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Data.Map as Map
import Util

type ChannelMap k v = TMVar (Map k (TChan v))

newChannelMap :: IO (ChannelMap k v)
newChannelMap = newTMVarIO Map.empty

openChannel :: Ord k => ChannelMap k v -> k -> IO (TChan v)
openChannel tm k = atomically $ do
    m <- takeTMVar tm
    (v, m') <- lookupOrInsertM k newBroadcastTChan m
    putTMVar tm m'
    dupTChan v
