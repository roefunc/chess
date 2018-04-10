module Chess (
    doTurn,
    doTurnAndGetState,
    playGame,
    createDefaultGame,
    allPossibleGames,
    allPossibleMoves,
    moveIsValid,
    isCheck,
    isVictory,
    Colour(..),
    createDefaultGameContext,
    createGameContext,
    findBetterPlayer
) where

import Data.List
import Control.Monad
import ChessTypes
import ChessUtils
import Game
import Data.Time.Clock
import qualified Data.Map as Map

type MoveMap = Map.Map PieceType [Piece->Game->[Move]]

createGameContext::Game->GameContext
createGameContext game = GameContext { getGame=game,moveListGenerator=(normalMoveListMap Map.!),checkListGenerator=(checkMoveListMap Map.!),moveHistory=[],gameHistory=[]}

createDefaultGameContext::GameContext
createDefaultGameContext = createGameContext createDefaultGame

-- Central move generating functions
moveIf::MovePredicate->TakePredicate->Int->Int->Piece->Game->[Move]
moveIf _ _ 0 0 _ _ = []
moveIf pred1 pred2 rMov cMov p@(Piece _ _ row col) game = let
                                                             rowInc = inc rMov
                                                             colInc = inc cMov
                                                             rowLim = row + rMov + rowInc
                                                             colLim = col + cMov + colInc

                                                             mov nr nc acc
                                                                          | nr < 0 || nc < 0 = acc
                                                                          | nr >= boardSize || nc >= boardSize = acc
                                                                          | nr == rowLim && nc == colLim = acc
                                                                          | pred1Val = mov (nr+rowInc) (nc+colInc) pred1RetVal
                                                                          | pred2 p np = ((row,col),(nr,nc)):acc
                                                                          | otherwise = acc
                                                                          where
                                                                            np = pieceOnSquare nr nc game
                                                                            (pred1Val,pred1Keep) = pred1 np
                                                                            pred1RetVal = if pred1Keep then (((row,col),(nr,nc)):acc) else acc
                                                           in
                                                             mov (row+rowInc) (col+colInc) []

ignoreEmpty::MovePredicate
ignoreEmpty _ = (False,False)

ignoreTake::TakePredicate
ignoreTake _ _ = False

jumpIf::MovePredicate->TakePredicate->Int->Int->Piece->Game->[Move]
jumpIf _ _ 0 0 _ _ = []
jumpIf pred1 pred2 rMov cMov p@(Piece _ _ row col) game = let
                                                               newRow = row + rMov
                                                               newCol = col + cMov
                                                               mov acc
                                                                 | newRow < 0 || newCol < 0 = []
                                                                 | newRow >= boardSize || newCol >= boardSize = []
                                                                 | pred1Val = acc
                                                                 | pred2 p np = acc
                                                                 | otherwise = []
                                                                 where
                                                                   np = pieceOnSquare newRow newCol game
                                                                   (pred1Val,_) = pred1 np
                                                              in
                                                                mov [((row,col),(newRow,newCol))]

forwardMult::Piece->Int
forwardMult (Piece _ White _ _) = 1
forwardMult (Piece _ Black _ _) = -1

forwardMoveIf::MovePredicate->TakePredicate->Int->Int->Piece->Game->[Move]
forwardMoveIf pred1 pred2 rMov cMov p game = moveIf pred1 pred2 ((forwardMult p)*rMov) cMov p game

forwardMoveIfOnRow::MovePredicate->TakePredicate->Int->Int->Int->Piece->Game->[Move]
forwardMoveIfOnRow pred1 pred2 rMov cMov row p@(Piece _ colour pRow _) game = let
                                                                                colourRow = case colour of
                                                                                         Black -> (boardSize-1)-row
                                                                                         White -> row
                                                                              in
                                                                                if (pRow==colourRow) then
                                                                                  forwardMoveIf pred1 pred2 rMov cMov p game
                                                                                else
                                                                                  []

-- Moves for all pieces
moveListForPiece::MovePredicate->TakePredicate->PieceType->[Piece->Game->[Move]]
moveListForPiece isEmptyP canTakeP Pawn = [(forwardMoveIf isEmptyP ignoreTake 1 0 ), (forwardMoveIf ignoreEmpty canTakeP 1 1), (forwardMoveIf ignoreEmpty canTakeP 1 (-1)), (forwardMoveIfOnRow isEmptyP ignoreTake 2 0 1)]
moveListForPiece isEmptyP canTakeP Rook = [moveIf isEmptyP canTakeP r c  | r <- [7,-7, 0], c <- [7,-7,0], (abs r) /= (abs c)]
moveListForPiece isEmptyP canTakeP Bishop = [moveIf isEmptyP canTakeP r c  | r <- [7,-7], c <- [7,-7]]
moveListForPiece isEmptyP canTakeP Queen = [moveIf isEmptyP canTakeP r c  | r <- [7,-7, 0], c <- [7,-7, 0]]
moveListForPiece isEmptyP canTakeP King =  [moveIf isEmptyP canTakeP r c  | r <- [1,-1, 0], c <- [1,-1, 0]]
moveListForPiece isEmptyP canTakeP Knight = [jumpIf isEmptyP canTakeP r c | r <- [2,-2,1,-1], c <- [2,-2,1,-1], (abs r) /= (abs c)]

-- Predicate that returns a pair of bools
-- First is whether the piece is nothing, second is whether
-- the containing square should be kept as a potential move,
-- true for normal moves, false for checks
-- This is an optimsation to reduce the number of squares contained in
-- potential check list
pieceIsNothing::(Maybe Piece)->(Bool,Bool)
pieceIsNothing Nothing = (True,True)
pieceIsNothing _ = (False,True)

pieceIsNothingCheck::(Maybe Piece)->(Bool,Bool)
pieceIsNothingCheck Nothing = (True,False)
pieceIsNothingCheck _ = (False,False)

-- take the two predicates required to evaluate moves
-- and generate Map from Piece to potential moves
-- used to generate checks and normal moves. This is an
-- optimisation to avoid rerunning moveListForPiece
createMoveMap::MovePredicate->TakePredicate->MoveMap
createMoveMap movePred takePred = foldl (\m p -> Map.insert p (moveListForPiece movePred takePred p) m) Map.empty [Pawn,Rook,Bishop,Queen,King,Knight]

normalMoveListMap::MoveMap
normalMoveListMap = createMoveMap pieceIsNothing canTake

checkMoveListMap::MoveMap
checkMoveListMap = createMoveMap pieceIsNothingCheck canCheck

-- Select correct move list and create moves from it
baseMovesForPiece::MoveListGenerator->Piece->Game->[Move]
baseMovesForPiece moveGen p@(Piece pt _ _ _) game = let
                         moveList = moveGen pt
                       in
                         concat [f p game | f <- moveList]


baseAllPossibleMoves::Game->(Piece->Game->[Move])->Colour->[Move]
baseAllPossibleMoves game movesf colour = concat $ do
                                 piece <- gameToPieceList game
                                 let (Piece _ pieceColour _ _) = piece
                                 guard (pieceColour == colour)
                                 let moves = movesf piece game
                                 return (nub moves)

allPossibleMoves::GameContext->Colour->[Move]
allPossibleMoves gameC colour = baseAllPossibleMoves (getGame gameC) (baseMovesForPiece (moveListGenerator gameC)) colour

allPossibleChecks::GameContext->Colour->[Move]
allPossibleChecks gameC colour = baseAllPossibleMoves (getGame gameC) (baseMovesForPiece (checkListGenerator gameC)) colour

allPossibleCheckSquares::GameContext->Colour->[(Int,Int)]
allPossibleCheckSquares gameC colour = map snd $ allPossibleChecks gameC colour

allPossibleGames::GameContext->Colour->[(Move,GameContext)]
allPossibleGames gameC colour = let
                                 possibleMoves = allPossibleMoves gameC colour
                               in
                                 [(m, newGameC) | m <- possibleMoves, let newGameC = movePieceWithMove m gameC, not $ isCheck newGameC colour]

moveIsValid::GameContext->Move->Colour->Bool
moveIsValid gameC move colour = (elem move $ allPossibleMoves gameC colour) && not (isCheck (movePieceWithMove move gameC) colour)

-- Predicate that returns true if the specified colour's
-- king is in check in the passed in game
isCheck::GameContext->Colour->Bool
isCheck gameC colour = elem kingSquare $ allPossibleCheckSquares gameC (oppColour colour)
                      where
                       kingSquare = findFirstPiece King colour (getGame gameC)

-- Predicate that returns true if the specified colour player
-- has any legal moves
canMove::GameContext->Colour->Bool
canMove gameC colour = not . null $ allPossibleGames gameC colour

-- Predicate that returns true if the supplied game is victory for the
-- specified player
isVictory::GameContext->Colour->State
isVictory gameC c = if canMove gameC colour then
                      if (gameIsDraw (gameHistory gameC)) then Stalemate else Normal
                    else
                      if (isCheck gameC colour) then Victory else Stalemate
                    where
                      colour = oppColour c

doTurnAndGetState::GameContext->Player->Colour->IO (GameContext, Move, State, Bool)
doTurnAndGetState gameC player colour = do
                              move <- player gameC
                              case move of
                                Just m@((oR,oC), (nR,nC)) -> do
                                                            case (moveIsValid gameC m colour) of
                                                              False -> error ("Invalid move from player " ++ (show colour))
                                                              _ -> do
                                                                    let newGameC = movePieceWithMove m gameC
                                                                    let check = isCheck newGameC (oppColour colour)
                                                                    return (newGameC, m, (isVictory newGameC colour), check)
                                _ -> return (gameC, ((0,0),(0,0)), (isVictory gameC colour), False)

doTurn::GameContext->Player->Colour->IO GameContext
doTurn gameC player colour = do
                              -- startTime <- getCurrentTime
                              move <- player gameC
                              case move of
                                Just m@((oR,oC), (nR,nC)) -> do
                                                            endTime <- getCurrentTime
                                                            -- putStrLn $ "Player " ++ (show colour) ++ " moves " ++ (show oR) ++ "," ++ (show oC) ++ " to " ++ (show nR) ++ "," ++ (show nC)
                                                            -- putStrLn $ "Time to calculate move " ++ (show (diffUTCTime endTime startTime)) ++ "s"
                                                            case (moveIsValid gameC m colour) of
                                                              False -> error ("Invalid move from player " ++ (show colour))
                                                              _ -> do
                                                                    let newGameC = movePieceWithMove m gameC
                                                                    if (isCheck newGameC (oppColour colour)) then putStrLn("Check!") else return ()
                                                                    return newGameC
                                _ -> return gameC

checkVictory::GameContext->Colour->IO State
checkVictory gameC colour = do
                             let s = isVictory gameC colour
                             case s of
                               Victory -> putStrLn ((show colour) ++ " Wins!")
                               Stalemate -> putStrLn "Draw!"
                               _ -> return ()
                             return s

playGame::GameContext->Player->Player->Colour->IO()
playGame gameC player1 player2 colour = do
                            newGame <- doTurn gameC player1 colour
                            showGame (getGame newGame)
                            vw <- checkVictory newGame colour
                            case vw of
                                Normal -> playGame newGame player2 player1 (oppColour colour)
                                _ -> return ()

pieceValue::Piece->Int
pieceValue (Piece Pawn _ _ _) = 100
pieceValue (Piece Knight _ _ _) = 300
pieceValue (Piece Bishop _ _ _) = 300
pieceValue (Piece Rook _ _ _) = 500
pieceValue (Piece Queen _ _ _) = 900
pieceValue (Piece King _ _ _) = 0

scorePiece::Piece->Colour->Int
scorePiece p@(Piece _ pieceColour _ _) colour = scoreMult * (pieceValue p)
                                                where
                                                  scoreMult = if (pieceColour == colour) then 1 else -1

selectBetterPlayerOnPoints::Colour->GameContext->Colour
selectBetterPlayerOnPoints colour gameC = if score > 0 then colour else (oppColour colour)
                                  where
                                    score = foldGame (\s p -> s+(scorePiece p colour)) 0 (getGame gameC)

findBetterPlayer::GameContext->Player->Player->Colour->Int->IO Colour
findBetterPlayer gameC _ _ colour 0 = return (selectBetterPlayerOnPoints colour gameC)
findBetterPlayer gameC player1 player2 colour turns = do
                            newGame <- doTurn gameC player1 colour
                            --showGame (getGame newGame)
                            vw <- checkVictory newGame colour
                            -- putStrLn (show vw)
                            case vw of
                                Normal -> findBetterPlayer newGame player2 player1 (oppColour colour) (turns-1)
                                Victory -> return colour
                                _ -> return (selectBetterPlayerOnPoints colour newGame)
