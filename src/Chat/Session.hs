module Chat.Session where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Chat.Packet
import Data.Aeson
import Network.WebSockets as WS


data UserState = UserState {
    userStateAlias :: String,
    userStateLat :: Double,
    userStateLon :: Double,
    chan :: TChan ServerPacket,
    con :: WS.Connection
}

type ChatSession a = StateT UserState IO a

getAlias :: ChatSession String
getAlias = userStateAlias <$> get

getPosition :: ChatSession (Double, Double)
getPosition = (userStateLat &&& userStateLon) <$> get

getChannel :: ChatSession (TChan ServerPacket)
getChannel = chan <$> get

getConnection :: ChatSession WS.Connection
getConnection = con <$> get

recvChannel :: ChatSession ServerPacket
recvChannel = getChannel >>= liftIO . atomically . readTChan

sendChannel :: ServerPacket -> ChatSession ()
sendChannel p = getChannel >>= liftIO . atomically . flip writeTChan p

recvClient :: ChatSession ClientPacket
recvClient = do
    con <- getConnection
    msg <- liftIO $ receiveDataMessage con
    case msg of
        Text txt -> case decode' txt of
            Just pack -> return pack
            otherwise -> recvClient
        otherwise -> recvClient

sendClient :: ServerPacket -> ChatSession ()
sendClient p = getConnection >>= liftIO . flip sendTextData (encode p)

runSession :: ChatSession a -> UserState -> IO a
runSession = evalStateT
