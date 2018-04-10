module HumanPlayer (
    humanPlayer
) where


import ChessTypes
import ChessUtils
import Chess

getIntList::String->Int->IO [Int]
getIntList message len = do
                                putStrLn message
                                pieceToMove <- getLine
                                let ws = (words pieceToMove)
                                case ((length ws) == len) of
                                    False -> do
                                                putStrLn "Invalid square"
                                                getIntList message len
                                    True -> do
                                                return (map (\x -> read x::Int) ws)

humanPlayer::Colour->GameContext->IO (Maybe Move)
humanPlayer colour gameC = do
                                [oR, oC] <- getIntList "Enter piece to move" 2
                                [nR, nC] <- getIntList "Enter square to move to" 2
                                case (moveIsValid gameC ((oR,oC), (nR,nC)) colour) of
                                    False -> do
                                                putStrLn "Invalid Move!"
                                                humanPlayer colour gameC
                                    True -> return (Just ((oR,oC),(nR,nC)))
