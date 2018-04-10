module Game (
   emptyGame,
   filterByColour,
   addPiecesToGame,
   removePieceOnSquare,
   doMove,
   addPieceToGame,
   pieceOnSquare,
   Game,
   gameToPieceList,
   foldGame,
   findFirstPiece
) where

import ChessTypes
import qualified Data.IntMap as IntMap

type Game = IntMap.IntMap Piece

emptyGame::Game
emptyGame = IntMap.empty

filterByColour::Colour->Game->Game
filterByColour c g = IntMap.filter (\(Piece _ co _ _) -> co == c) g

addPiecesToGame::[Piece]->Game->Game
addPiecesToGame ps game = foldl (\g p -> addPieceToGame p g) game ps

rowColToIndex::Int->Int->Int
rowColToIndex r c = r*8 + c

removePieceOnSquare::Int->Int->Game->Game
removePieceOnSquare row col game = IntMap.delete (rowColToIndex row col) game

doMove :: Move -> Game -> Game
doMove ((oldRow,oldCol), (newRow,newCol)) game = let
                                                   i = rowColToIndex oldRow oldCol
                                                   (Piece t c _ _) = (game IntMap.! i)
                                                 in
                                                   IntMap.insert (rowColToIndex newRow newCol) (Piece t c newRow newCol) $ IntMap.delete i game

addPieceToGame::Piece->Game->Game
addPieceToGame p@(Piece _ _ r c) game = IntMap.insert (rowColToIndex r c) p game

pieceOnSquare::Int->Int->Game->Maybe Piece
pieceOnSquare r c game = IntMap.lookup (rowColToIndex r c) game

gameToPieceList::Game->[Piece]
gameToPieceList g = IntMap.elems g

foldGame::(b->Piece->b)->b->Game->b
foldGame f i g = IntMap.foldl f i g

findFirstPiece::PieceType->Colour->Game->(Int,Int)
findFirstPiece t c g = (row,col)
                    where
                        (_,(Piece _ _ row col)) = IntMap.findMin $ IntMap.filter (\(Piece pt pc _ _) -> pt == t && pc == c) g
