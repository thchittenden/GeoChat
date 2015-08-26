module Chat where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text
import Data.Map as Map
import GHC.Generics
import Network.Wai
import Network.HTTP.Types
import Network.WebSockets as WS
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Servant
import System.IO.Unsafe
import Util
import Chat.ChannelMap
import Chat.Session
import Chat.Packet
import Chat.Room

type ChatAPI = "chat" :> Capture "alias" String :> Capture "lat" Double :> Capture "lon" Double :> Raw

chatApi :: Proxy ChatAPI
chatApi = Proxy

chatServer :: Server ChatAPI
chatServer = chatroom

-- This is pretty unsafe/unsound (Who runs this? When is it run!?) but unless
-- we want initialization to bubble up to main, this is how it's gonna be.
{-# NOINLINE chatroomChannel #-}
chatroomChannel :: TChan ServerPacket
chatroomChannel = unsafePerformIO newTChanIO

{-# NOINLINE chatroomUsers #-}
chatroomUsers :: ChatRoom
chatroomUsers = unsafePerformIO (newChatRoom "default" chatroomChannel)

-- Sends data to the client received over the channel.
chatroomSend :: ChatSession ()
chatroomSend = forever $ recvChannel >>= sendClient

-- Sends data to the channel from the client.
chatroomRecv :: ChatSession ()
chatroomRecv = forever $ do
    alias <- getAlias
    clientPacket <- recvClient
    case clientPacket of
        ClientMessage msg -> sendChannel (ServerMessage alias msg)

sendUsers :: ChatSession ()
sendUsers = do
    users <- liftIO $ getUsers chatroomUsers
    sendClient (ServerInitUsers users)

-- Not much to do on a connection exception. Just log it. Usually
-- these are just ConnectionClosed exceptions.
chatroomExHandler :: ConnectionException -> IO ()
chatroomExHandler ex = putStrLn ("Exception: " ++ show ex)

-- Accepts a pending connection and routes messages to/from the channel.
chatroomApp :: String -> Double -> Double -> WS.PendingConnection -> IO ()
chatroomApp alias lat lon pcon = do
    con <- acceptRequest pcon
    putStrLn ("Connection from " ++ alias ++ " at " ++ "(" ++ show lat ++ ", " ++ show lon ++ ")")
    let state = UserState alias lat lon chatroomChannel con
        user = ChatUser alias lat lon
        send = runSession chatroomSend state
        recv = runSession chatroomRecv state
    addUser chatroomUsers user
    runSession sendUsers state
    bracket
        (forkIO $ catch send chatroomExHandler) killThread
        (\____ -> catch recv chatroomExHandler)
    removeUser chatroomUsers user

chatroom :: String -> Double -> Double -> Application
chatroom alias lat lon = websocketsOr ops (chatroomApp alias lat lon) defaultApp
    where ops = defaultConnectionOptions
          defaultApp _ resp = resp (responseLBS imATeaPot418 [] "")
