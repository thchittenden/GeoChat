module Chat where

import Chat.Packet
import Chat.Room
import Chat.Session
import Chat.Session.Action
import Chat.Types
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
chatServer alias lat lon = websocketsOr ops (chatroomApp alias lat lon) defaultApp
    where ops = defaultConnectionOptions
          defaultApp _ resp = resp (responseLBS imATeaPot418 [] "")

-- This is pretty unsafe/unsound (Who runs this? When is it run!?) but unless
-- we want initialization to bubble up to main, this is how it's gonna be.
{-# NOINLINE rootChatroom #-}
rootChatroom :: ChatRoom
rootChatroom = unsafePerformIO (newChatRoom "root")

-- Accepts a pending connection and starts a new session.
chatroomApp :: String -> Double -> Double -> WS.PendingConnection -> IO ()
chatroomApp alias lat lon pcon = do
    con <- acceptRequest pcon
    sess <- newSession (ChatUser alias lat lon) con rootChatroom
    waitSession sess
