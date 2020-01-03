module Entities where 

import Data.Map (Map)
import qualified Data.Map as Map

data Message = NULL | Message {
    coord :: Cell,
    result :: String,
    next :: Message
} deriving Show

type Cell = (String, String)

type ServerState = Map String Message

