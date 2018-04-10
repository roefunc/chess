module ChessTypes (
    Colour(..),
    PieceType(..),
    Piece(..),
    Move,
    State(..)
) where

import Control.DeepSeq

data Colour = Black | White deriving (Show,Eq,Ord)
data PieceType = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show,Eq,Ord)
data Piece = Piece PieceType Colour Int Int deriving (Eq,Show,Ord)
-- Function that takes a game and returns a move
type Move = ((Int,Int),(Int,Int))
data State = Normal | Victory | Stalemate deriving (Eq,Show)

instance NFData Piece where
    rnf (Piece _ _ r c) = rnf r `seq` rnf c
