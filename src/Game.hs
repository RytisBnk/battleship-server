module Game where

import Entities
import EntityHelpers
import BencodeParser

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Tuple

board :: [Cell]
board = [("C", "5"), ("D", "5"), ("E", "5"), ("F", "5"),
        ("B", "8"), ("C", "8"), ("D", "8"),   ("I", "2"), ("I", "3"), ("I", "4"),
        ("B", "1"), ("B", "2"),   ("C", "10"), ("D", "10"),   ("J", "7"), ("J", "8"),
        ("A", "6"),   ("D", "3"),   ("F", "2"),   ("H", "10")]

playGame :: String -> IO (Message)
playGame requestBody = do
    case parseBencode requestBody of
        Left error -> return NULL
        Right (newMessage, rest) -> 
            case checkEndCondition newMessage "B" of
                Left error -> do
                    putStrLn error
                    return NULL
                Right (conditionReached, winner) -> do
                    let prevResult = getOpponentShotResult newMessage
                    let prevTargets = snd $ getPlayerTargets newMessage
                    if conditionReached then
                        if winner == "B" then
                        return $ mconcat [newMessage, (Message ("0", "0") "HIT" NULL)]
                        else do
                            responseMessage <- getRandomShotResponse prevTargets prevResult
                            return $ mconcat [newMessage, responseMessage]
                    else do
                        responseMessage <- getRandomShotResponse prevTargets prevResult
                        return $ mconcat [newMessage, responseMessage]
            

getPlayerTargets :: Message -> ([Cell], [Cell])
getPlayerTargets message = getPlayerTargets' message 0 ([], [])
    where
        getPlayerTargets' :: Message -> Int -> ([Cell], [Cell]) -> ([Cell], [Cell])
        getPlayerTargets' NULL msgCount targets = targets
        getPlayerTargets' (Message coord result next) msgCount (targetsA, targetsB) =
            if msgCount `mod` 2 == 0 then
                getPlayerTargets' next (msgCount + 1) (targetsA ++ [coord], targetsB)
            else 
                getPlayerTargets' next (msgCount + 1) (targetsA, targetsB ++ [coord])

getOpponentShotResult :: Message -> String 
getOpponentShotResult message = 
    let 
        lastMessage = getLastMessage message 
    in
        if (coord lastMessage) `elem` board then "HIT"
        else "MISS"

getRandomShotResponse :: [Cell] -> String -> IO (Message)
getRandomShotResponse prevTargets prevResult = do
    randomCoord <- getRandomCoord
    if randomCoord `elem` prevTargets then
        getRandomShotResponse prevTargets prevResult
    else 
        return $ Message randomCoord prevResult NULL

checkEndCondition :: Message -> String -> Either String (Bool, String)
checkEndCondition message player = 
    case countScore message of
        Left error -> Left error
        Right (hitsA, hitsB) ->
            let
                previousResult = getOpponentShotResult message
                (scoreA, scoreB) = if previousResult == "MISS" then (hitsA, hitsB)
                    else case player of
                        "A" -> (hitsA, hitsB + 1)
                        "B" -> (hitsA + 1, hitsB)
            in if scoreA == 20 then Right (True, "A")
                else if scoreB == 20 then Right (True, "B")
                else Right (False, "")


countScore :: Message -> Either String (Int, Int)
countScore message = 
    if isMessageValid message 
        then countScore' message 0 0 0
    else Left ("Only one move can be preformed for each individual cell" ++ show message)
    where 
        countScore' :: Message -> Int -> Int -> Int -> Either String (Int, Int)  -- message, number of prior moves, hits A, hits B
        countScore' NULL _ hitsA hitsB = Right (hitsA, hitsB)
        countScore' message count hitsA hitsB = 
            case result message of
                "HIT" ->
                    case mod count 2 of
                        0 -> countScore' (Entities.next message) (count + 1) hitsA (hitsB + 1)
                        1 -> countScore' (Entities.next message) (count + 1) (hitsA + 1) hitsB
                "MISS" -> countScore' (Entities.next message) (count + 1) hitsA hitsB
                "" -> 
                    if count == 0 then
                        countScore' (Entities.next message) (count + 1) hitsA hitsB
                    else Left ("Result undefined for message that isn't first " ++ (show message))

isMessageValid :: Message -> Bool
isMessageValid message = validateMessage' message [] [] 0
    where
        validateMessage' :: Message -> [(String, String)] -> [(String, String)] -> Int -> Bool
        validateMessage' NULL _ _ _ = True
        validateMessage' message previousHitsA previousHitsB previousMoves = 
            case mod previousMoves 2 of 
                0 ->
                    if coord message `elem` previousHitsA 
                        then False
                        else validateMessage' (Entities.next message) (previousHitsA ++ [coord message]) previousHitsB (previousMoves + 1)
                1 -> 
                    if coord message `elem` previousHitsB 
                        then False
                        else validateMessage' (Entities.next message) previousHitsA (previousHitsB ++ [coord message]) (previousMoves + 1)