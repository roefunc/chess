{-# LANGUAGE OverloadedStrings #-}

module ChessHandler (
                    chessHandler
) where

import HttpTypes
import Http
import Data.Int
import System.Posix.Types
import Chess
import ComputerPlayer
import ChessUtils
import ChessTypes
import Game
import PlayerGen
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Debug.Trace

chessHandler::Handler
chessHandler = Handler { sendData=sendGame }

pieceTypeString::PieceType->String
pieceTypeString Pawn = "pawn"
pieceTypeString Knight = "knight"
pieceTypeString Bishop = "bishop"
pieceTypeString Rook = "rook"
pieceTypeString Queen = "queen"
pieceTypeString King = "king"

colourString::Colour->String
colourString Black = "black"
colourString White = "white"

stateString::State->String
stateString Victory = "victory"
stateString Stalemate = "stalemate"
stateString Normal = "normal"

pieceTypeFromString::String->PieceType
pieceTypeFromString "pawn" = Pawn
pieceTypeFromString "knight" = Knight
pieceTypeFromString "bishop" = Bishop
pieceTypeFromString "rook" = Rook
pieceTypeFromString "queen" = Queen
pieceTypeFromString "king" = King

colourFromString::String->Colour
colourFromString "white" = White
colourFromString "black" = Black

data GetMoveCommand = GetMoveCommand { getDepth::Int, getColour::Colour, getGameContext::GameContext }
data MakeMoveCommand = MakeMoveCommand { getMove::Move, getMakeColour::Colour, getMakeGameContext::GameContext }

instance FromJSON Piece where
  parseJSON = withObject "piece" $ \o ->
    do
    pieceType <- o .: "type"
    side <- o .: "side"
    col <- o .: "col"
    row <- o .: "row"
    return (Piece (pieceTypeFromString pieceType) (colourFromString side) row col)

instance FromJSON GetMoveCommand where
  parseJSON = withObject "getmovecommand" $ \o -> do
    depth <- o .: "depth"
    side <- o .: "side"
    pieces <- o .: "gameState"
    return (GetMoveCommand { getDepth = depth, getColour=(colourFromString side), getGameContext=(createGameContext $ addPiecesToGame pieces emptyGame) })

moveFromArray::[[Int]]->Move
moveFromArray [[r1,c1],[r2,c2]] = ((r1,c1),(r2,c2))
moveFromArray _ = ((0,0),(0,0))

instance FromJSON MakeMoveCommand where
  parseJSON = withObject "makemovecommand" $ \o -> do
    move <- o .: "move"
    side <- o .: "side"
    pieces <- o .: "gameState"
    return (MakeMoveCommand {getMove = moveFromArray move, getMakeColour=(colourFromString side), getMakeGameContext=(createGameContext $ addPiecesToGame pieces emptyGame) })

instance ToJSON Piece where
  toJSON (Piece pieceType colour row col) =
    object [ "col" .= col,
             "row" .= row,
             "type" .= (pieceTypeString pieceType),
             "side" .= (colourString colour)
             ]

getMoves::[(Move,GameContext)]->[Move]
getMoves l = let
              go ((m,g):rest) acc = go rest (m:acc)
              go [] acc = acc
             in
              go l []

stringifyGame::(GameContext,Move,State,Bool)->Colour->B.ByteString
stringifyGame (game, move, state, check) colour = encode $ object [
                      "allowedMoves" .= (getMoves $ allPossibleGames game (oppColour colour)),
                      "move"  .= move,
                      "pieces" .= gameToPieceList (getGame game),
                      "state" .= stateString state,
                      "check" .= check
                      ]

emptyMove::Move
emptyMove = ((-1,-1),(-1,-1))

initialState::B.ByteString
initialState = stringifyGame (createDefaultGameContext, emptyMove, Normal, False) Black

sendState::Sender->B.ByteString->String->IO Int64
sendState f state name = do
                      B.putStrLn state
                      let len = COff (fromIntegral(B.length state)::Int64)
                      f (OK "application/json" name len) state

getMoveCommand::String->Maybe GetMoveCommand
getMoveCommand string = decode (packStr string)

getMakeMoveCommand::String->Maybe MakeMoveCommand
getMakeMoveCommand string = decode (packStr string);

getMoveResponse::String->IO B.ByteString
getMoveResponse input = case (getMoveCommand input) of
                          Just (GetMoveCommand {getDepth=depth,getColour=colour, getGameContext=gameContext}) -> do
                            nextGameState <- getNextGameState gameContext colour depth
                            return (stringifyGame nextGameState colour)
                          _ -> return ""

makeMoveResponse::String->B.ByteString
makeMoveResponse input = case (getMakeMoveCommand input) of
                          Just (MakeMoveCommand { getMove=move,getMakeColour=colour,getMakeGameContext=gameContext}) -> stringifyGame ((movePieceWithMove move gameContext), emptyMove, isVictory gameContext colour, isCheck gameContext colour) colour
                          _ -> ""

sendGame::Sender->Config->HttpCommand->String->IO Int64
sendGame f _ command name = case name of
                      "initial" -> sendState f initialState name
                      "getmove" -> do
                                    moveResponse <- getMoveResponse $ commandContents command
                                    sendState f moveResponse name
                      "makemove" -> do
                                    putStrLn $ commandContents command
                                    let moveResponse = makeMoveResponse $ commandContents command
                                    sendState f moveResponse name
                      _ -> f (OK "text/plain" "" 1) (packStr " ")

readParamList::String->IO ParamList
readParamList path = do
                    contents <- readFile path
                    return (read contents::ParamList)

getNextGameState::GameContext->Colour->Int->IO (GameContext, Move, State, Bool)
getNextGameState gameContext colour depth =  do
             paramList <- readParamList "best.input"
             let compPlayer = createComputerPlayer paramList depth colour
             doTurnAndGetState gameContext compPlayer colour
