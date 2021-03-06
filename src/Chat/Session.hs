module Chat.Session where

import Chat.Room
import Chat.Session.Action
import Chat.Types
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Chat.Packet
import Network.WebSockets as WS

newSession :: ChatUser -> WS.Connection -> ChatRoom -> IO ChatSession
newSession user con chatroom = do
    putStrLn ("New Session: " ++ show user)
    sessBox <- newEmptyTMVarIO
    sendTid <- forkIO $ sendThread sessBox
    recvTid <- forkIO $ recvThread sessBox
    chatroomVar <- newTVarIO chatroom
    exited <- newEmptyTMVarIO
    let sess = ChatSession sendTid recvTid exited (alias user) (lat user) (lon user) chatroomVar con
    addSession chatroom sess
    atomically $ putTMVar sessBox sess
    runAction (forkPingAction 5) sess
    return sess

-- The send thread channels messages from the chatroom channel to the client connection.
sendThread :: TMVar ChatSession -> IO ()
sendThread sessBox = do
    sess <- atomically $ readTMVar sessBox
    handle sendHandler $ runAction sendAction sess

sendHandler :: SomeException -> IO ()
sendHandler ex = putStrLn ("Send Exception: " ++ show ex)

sendAction :: SessionAction ()
sendAction = do
    chatroom <- getChatroom
    users <- liftIO $ getUsers chatroom
    sendClient (ServerInitUsers users)
    forever $ recvChannel >>= sendClient

-- The receive thread channels messages from the client connection to the
-- chatroom channel. When the client closes the connection (throwing a
-- ConnectionClosed exception, this thread will kill the sending thread and
-- notify any waiters that they've exited).
recvThread :: TMVar ChatSession -> IO ()
recvThread sessBox = do
    sess <- atomically $ readTMVar sessBox
    handle (recvHandler sess) $ runAction recvAction sess

recvHandler :: ChatSession -> SomeException -> IO ()
recvHandler sess ex = do
    putStrLn ("Recv Exception: " ++ show ex)
    throwTo (sendTid sess) SessionEnded
    atomically $ putTMVar (exited sess) ()

recvAction :: SessionAction ()
recvAction = forever $ do
    alias <- getAlias
    clientPacket <- recvClient
    case clientPacket of
        ClientMessage msg -> sendChannel (ServerMessage alias msg)

-- Creates a thread that sends pings every n seconds to the
-- client. This keeps sockets from closing on some browsers.
forkPingAction :: Int -> SessionAction ()
forkPingAction n = do
    con <- getConnection
    liftIO $ forkPingThread con n

-- Waits for the client to end the session.
waitSession :: ChatSession -> IO ()
waitSession sess = do
    atomically $ readTMVar (exited sess)
    chatroom <- readTVarIO (chatroom sess)
    removeSession chatroom sess
    putStrLn $ "Session ended: " ++ show (getUser sess)

-- Forcibly closes the connection.
killSession :: ChatSession -> IO ()
killSession sess = do
    killThread (sendTid sess)
    killThread (recvTid sess)
    waitSession sess
