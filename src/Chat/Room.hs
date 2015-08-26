module Chat.Room where

import Control.Concurrent.STM
import Control.Monad
import qualified Chat.Packet as P
import qualified Data.Map as Map

data ChatRoom = ChatRoom {
    name :: String,
    chan :: TChan P.ServerPacket,
    users :: TVar (Map.Map String P.ChatUser)
}

newChatRoom :: String -> TChan P.ServerPacket -> IO ChatRoom
newChatRoom name chan = liftM (ChatRoom name chan) (newTVarIO Map.empty)

addUser :: ChatRoom -> P.ChatUser -> IO ()
addUser room user = do
    let alias = P.alias user
    putStrLn ("Adding " ++ alias ++ " to chatroom " ++ name room)
    atomically $ modifyTVar (users room) (Map.insert alias user)
    atomically $ writeTChan (chan room) (P.ServerAddUser user)

removeUser :: ChatRoom -> P.ChatUser -> IO ()
removeUser room user = do
    let alias = P.alias user
    putStrLn ("Removing " ++ alias ++ " from chatroom " ++ name room)
    atomically $ writeTChan (chan room) (P.ServerRemoveUser user)
    atomically $ modifyTVar (users room) (Map.delete alias)

getUsers :: ChatRoom -> IO [P.ChatUser]
getUsers room = atomically $ liftM Map.elems (readTVar (users room))
