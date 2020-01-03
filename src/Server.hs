{-# LANGUAGE OverloadedStrings #-}

module Server where

import Entities
import BencodeParser
import Game
import Encoder

import Web.Scotty
import Network.HTTP.Types

import Data.Monoid (mconcat)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L
import Data.ByteString.Lazy.Char8 as BS
import Data.Tuple

import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.STM.TVar
import GHC.Conc.Sync

initialState :: Map String (Message, Bool)
initialState = Map.fromList []

runServer = do
    serverState <- newTVarIO initialState
    scotty 8080 $ do
        get "/game/:gameId/player/:playerId" $ do
            gameId <- param "gameId"
            savedGames <- liftIO $ readTVarIO serverState
            let messageState = Map.findWithDefault (NULL, False) gameId savedGames
            if (snd messageState) then do
                liftIO $ atomically $ swapTVar serverState (Map.insert gameId (fst messageState, False) savedGames)
                raw $ BS.pack $ (encodeMessage $ fst messageState)
            else do
                raw $ BS.pack "No move available at the moment"
                status status409
        post "/game/:gameId/player/:playerId" $ do
            content <- body
            gameId <- param "gameId"
            savedGames <- liftIO $ readTVarIO serverState
            -- let lastMessage = Map.findWithDefault NULL gameId savedGames
            let bodyString = L.unpack $ decodeUtf8 content
            msg <- liftIO $ playGame bodyString
            liftIO $ atomically $ swapTVar serverState (Map.insert gameId (msg, True) savedGames)
            liftIO $ Prelude.putStrLn $ show msg
            raw $ BS.pack (encodeMessage msg)
            status status204


