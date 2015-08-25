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

type ChatAPI = "chat" :> Capture "alias" String :> Capture "lat" Double :> Capture "lon" Double :> Raw

chatApi :: Proxy ChatAPI
chatApi = Proxy

chatServer :: Server ChatAPI
chatServer = chatroom

data ChatMessage = ChatMessage {
    alias :: String,
    text :: String
} deriving Generic
instance FromJSON ChatMessage
instance ToJSON ChatMessage

-- This is pretty unsafe/unsound (Who runs this? When is it run!?) but unless
-- we want initialization to bubble up to main, this is how it's gonna be.
{-# NOINLINE channelMap #-}
channelMap :: TMVar (Map Integer (TChan ChatMessage))
channelMap = unsafePerformIO $ newTMVarIO Map.empty

getChannel :: Integer -> STM (TChan ChatMessage)
getChannel id = do
    m <- takeTMVar channelMap
    (v, m') <- lookupOrInsertM id newBroadcastTChan m
    putTMVar channelMap m'
    dupTChan v

-- Sends data to the client received over the channel.
chatroomSend :: String -> TChan ChatMessage -> WS.Connection -> IO ()
chatroomSend alias chan con = forever $ do
    msg <- atomically $ readTChan chan
    sendTextData con (encode msg)

-- Receives data from the client and sends over the channel.
chatroomRecv :: String -> TChan ChatMessage -> WS.Connection -> IO ()
chatroomRecv alias chan con = forever $ do
    msg <- receiveDataMessage con
    case msg of
        Text val -> atomically $ writeTChan chan (ChatMessage alias (BS.unpack val))
        otherwise -> putStrLn ("Invalid message: " ++ show msg)

-- Not much to do on a connection exception. Just log it. Usually
-- these are just ConnectionClosed exceptions.
chatroomExHandler :: ConnectionException -> IO ()
chatroomExHandler ex = putStrLn ("Exception: " ++ show ex)

-- Accepts a pending connection and routes messages to/from the channel.
chatroomApp :: String -> WS.PendingConnection -> IO ()
chatroomApp alias pcon = do
    chan <- atomically $ getChannel 0
    con <- acceptRequest pcon
    bracket
        (forkIO $ chatroomSend alias chan con)
        (killThread)
        (\_ -> catch (chatroomRecv alias chan con) chatroomExHandler)

chatroom :: String -> Double -> Double -> Application
chatroom alias lat lon = websocketsOr ops (chatroomApp alias) defaultApp
    where ops = defaultConnectionOptions
          defaultApp _ resp = resp (responseLBS imATeaPot418 [] "")
