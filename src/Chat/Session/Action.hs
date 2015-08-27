module Chat.Session.Action where

import Chat.Types
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Chat.Packet
import Data.Aeson
import Network.WebSockets as WS
import Util

type SessionAction a = StateT ChatSession IO a

getAlias :: SessionAction String
getAlias = userStateAlias <$> get

getPosition :: SessionAction (Double, Double)
getPosition = (userStateLat &&& userStateLon) <$> get

getConnection :: SessionAction WS.Connection
getConnection = con <$> get

getChatroom :: SessionAction ChatRoom
getChatroom = chatroom <$> get >>= (liftIO . readTVarIO)

withChannelDo' ::(TChan ServerPacket -> IO a) ->  TVar ChatRoom -> IO a
withChannelDo' action tvar = handle retry $ do
    chatroom <- liftIO $ readTVarIO tvar
    channel <- liftIO $ atomically $ dupTChan (channel chatroom)
    action channel
    where retry ChatroomChanged = withChannelDo' action tvar
          retry SessionEnded = throw SessionEnded

withChannelDo :: (TChan ServerPacket -> IO a) -> SessionAction a
withChannelDo action = chatroom <$> get >>= (liftIO . withChannelDo' action)

recvChannel :: SessionAction ServerPacket
recvChannel = withChannelDo (liftIO . atomically . readTChan)

sendChannel :: ServerPacket -> SessionAction ()
sendChannel p = withChannelDo (liftIO . atomically . flip writeTChan p)

recvClient :: SessionAction ClientPacket
recvClient = do
    con <- getConnection
    msg <- liftIO $ receiveDataMessage con
    case parseMessage msg of
        Just packet -> return packet
        Nothing -> recvClient

-- Parses and sanitizes a message. For some reason trying to sanitize the raw
-- JSON causes an infinite loop so we have to sanitize the fields individually.
-- Luckily that's easy when there's only one field!
parseMessage :: DataMessage -> Maybe ClientPacket
parseMessage msg = case msg of
    Text txt -> fmap sanitize (decode txt)
    otherwise -> Nothing
    where sanitize (ClientMessage msg) = ClientMessage (sanitizeString msg)

sendClient :: ServerPacket -> SessionAction ()
sendClient p = getConnection >>= liftIO . flip sendTextData (encode p)

runAction :: SessionAction a -> ChatSession -> IO a
runAction = evalStateT
