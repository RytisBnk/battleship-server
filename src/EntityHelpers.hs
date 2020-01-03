module EntityHelpers where

import Entities

import Data.Monoid
import Data.Semigroup

import qualified System.Random as R

instance Semigroup Message where
    message1 <> message2 = appendNewMessage message1 message2

instance Monoid Message where
    mempty = NULL

defaultMessage :: Message
defaultMessage = Message ("0", "0") "" NULL

getMessageList :: Message -> [Message] -- Messages are returned in reverse order (last to first)
getMessageList NULL = []
getMessageList message = getMessageList' message []
    where 
        getMessageList' :: Message -> [Message] -> [Message]
        getMessageList' (Message coord result NULL) msgList = [Message coord result NULL] ++ msgList
        getMessageList' (Message coord result next) msgList = getMessageList' next ([Message coord result NULL] ++ msgList)

appendNewMessage :: Message -> Message -> Message -- target message, appended message, result
appendNewMessage NULL appended = appended
appendNewMessage (Message coord result NULL) appended = Message coord result appended
appendNewMessage (Message coord result next) appended = Message coord result $ appendNewMessage next appended

reverseMessage :: Message -> Message 
reverseMessage message = mconcat $ getMessageList message

getLastMessage :: Message -> Message
getLastMessage NULL = NULL
getLastMessage (Message coord result NULL) = Message coord result NULL
getLastMessage message = getLastMessage $ (next message)

translateCoord :: (Int, Int) -> Cell
translateCoord (coordX, coordY) = 
    case coordX of
        1 -> ("A", show coordY)
        2 -> ("B", show coordY)
        3 -> ("C", show coordY)
        4 -> ("D", show coordY)
        5 -> ("E", show coordY)
        6 -> ("F", show coordY)
        7 -> ("G", show coordY)
        8 -> ("H", show coordY)
        9 -> ("I", show coordY)
        10 -> ("J", show coordY)

getRandomList :: Int -> IO([Int])
getRandomList 0 = return []
getRandomList n = do
    r  <- R.randomRIO (1,10)
    rs <- getRandomList (n-1)
    return (r:rs) 

getRandomCoord :: IO(Cell)
getRandomCoord = do
    randomCoord <- getRandomList 2
    let coord = (randomCoord !! 0, randomCoord !! 1)
    return $ translateCoord coord

message1 = Message {coord = ("C", "2"), result = "", next = NULL}
message2 = Message {coord = ("A", "3"), result = "HIT", next = NULL}
message3 = Message {coord = ("B", "6"), result = "MISS", next = NULL}

-- append1 = getMessageObject [message1, message2, message3]
-- append2 = mconcat [message1, message2, message3]

