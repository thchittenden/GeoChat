module Chat.Packet where

import Data.Aeson
import GHC.Generics


-- A user of the chat service.
data ChatUser = ChatUser { alias :: String, lat :: Double, lon :: Double } deriving (Show, Generic)
instance FromJSON ChatUser
instance ToJSON ChatUser

-- The packet type passed from the server to the client
data ServerPacket = ServerMessage { userAlias :: String, message :: String }
                  | ServerInitUsers { users :: [ChatUser] }
                  | ServerAddUser { user :: ChatUser }
                  | ServerRemoveUser { user :: ChatUser }
                  deriving Generic

instance FromJSON ServerPacket
instance ToJSON ServerPacket

-- The packet type passed from the client to the server.
data ClientPacket = ClientMessage { clientMessage :: String } deriving Generic
instance FromJSON ClientPacket
instance ToJSON ClientPacket
