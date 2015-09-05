import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Wai.Handler.Warp
import Network.Socket.Internal
import Servant

import Chat

type API = ChatAPI
      :<|> Raw

server :: Server API
server = chatServer
    :<|> serveDirectory "www"

port :: Int
port = 8081

main :: IO ()
main = runServer
    where settings =
            setPort port $
            setOnOpen onOpen
            defaultSettings
          api = Proxy :: Proxy API
          app = serve api server
          runServer = runSettings settings app

onOpen :: SockAddr -> IO Bool
onOpen addr = return True
