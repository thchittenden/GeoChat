-- Types used in the Chat module and general manipulation functions.
module Chat.Types where

import Chat.Packet
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import qualified Data.Map as Map
import Network.WebSockets as WS

data SessionException = ChatroomChanged
                      | SessionEnded
                      deriving Show

instance Exception SessionException

data ChatRoom = ChatRoom {
    -- An identifier for the chat room.
    name :: String,

    -- The channel on which chat room messages are broadcast.
    channel :: TChan ServerPacket,

    -- All sessions currently in the chat room.
    sessions :: TMVar (Map.Map String ChatSession)
}

getUsers :: ChatRoom -> IO [ChatUser]
getUsers room = do
    sessions <- atomically $ readTMVar (sessions room)
    return . map getUser . Map.elems $ sessions

data ChatSession = ChatSession {
    -- The thread IDs of the two IO threads used in the session.
    sendTid :: ThreadId,
    recvTid :: ThreadId,

    -- An event to indicate when the threads have exited.
    exited :: TMVar (),

    -- User information.
    userStateAlias :: String,
    userStateLat :: Double,
    userStateLon :: Double,

    -- This is the
    chatroom :: TVar ChatRoom,

    -- The session connection. This does not change as it's tied to the client.
    con :: WS.Connection
}

getUser :: ChatSession -> ChatUser
getUser sess = ChatUser (userStateAlias sess) (userStateLat sess) (userStateLon sess)
