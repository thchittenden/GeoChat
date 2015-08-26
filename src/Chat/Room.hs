module Chat.Room where

import Chat.Packet
import Chat.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map as Map

modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar tmvar action = do
    var <- takeTMVar tmvar
    putTMVar tmvar (action var)

{-
splitRoom :: (ChatSession -> Bool) -> ChatRoom -> IO (ChatRoom, ChatRoom)
splitRoom cond room = do
    -- Extract the sessions, partition them on the predicate, and replace the
    -- left half.
    sess <- atomically $ takeTMVar (sessions room)
    let (lsess, rsess) = Map.partition cond sess
    atomically $ putTMVar (sessions room) lsess

    -- Construct a new ChatRoom for the right half and notify the threads.
    newChan <- newTChanIO
    newSessions <- newTMVarIO rsess
    let newRoom = ChatRoom newChan newSessions
    forM_ (Map.elems rsess) $ \sess -> do
        atomically . flip writeTVar newChan . chan $ sess
        throwTo (tid sess) ChannelChanged

    -- Return the old and new chat room.
    return (room, newRoom)
-}

newChatRoom :: String -> IO ChatRoom
newChatRoom name = do
    chan <- newTChanIO
    sess <- newTMVarIO Map.empty
    return $ ChatRoom name chan sess

addSession :: ChatRoom -> ChatSession -> IO ()
addSession room sess = do
    let alias = userStateAlias sess
    putStrLn ("Adding " ++ alias ++ " to chatroom " ++ name room)
    atomically $ modifyTMVar (sessions room) (Map.insert alias sess)
    atomically $ writeTChan (channel room) (ServerAddUser (getUser sess))

removeSession :: ChatRoom -> ChatSession -> IO ()
removeSession room sess = do
    let alias = userStateAlias sess
    putStrLn ("Removing " ++ alias ++ " from chatroom " ++ name room)
    atomically $ writeTChan (channel room) (ServerRemoveUser (getUser sess))
    atomically $ modifyTMVar (sessions room) (Map.delete alias)
