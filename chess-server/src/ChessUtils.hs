module ChessUtils (
    createDefaultGame,
    emptyGame,
    addPieceToGame,
    addPiecesToGame,
    canTake,
    canCheck,
    showGame,
    inc,
    oppColour,
    gameToString,
    gameToPieceList,
    Player,
    boardSize,
    MovePredicate,
    TakePredicate,
    MoveListGenerator,
    movePieceWithMove,
    GameContext(..),
    gameIsDraw
) where

import ChessTypes
import Game

type Player = GameContext->IO (Maybe Move)
type MovePredicate = (Maybe Piece)->(Bool,Bool)
type TakePredicate = Piece->(Maybe Piece)->Bool

type MoveListGenerator = PieceType->[Piece->Game->[Move]]
data GameContext = GameContext { getGame::Game,moveListGenerator::MoveListGenerator,checkListGenerator::MoveListGenerator,moveHistory::[Move],gameHistory::[Game] }

boardSize::Int
boardSize=8

inc::Int->Int
inc i
    | i < 0 = -1
    | i > 0 = 1
    | otherwise = 0

oppColour::Colour->Colour
oppColour White = Black
oppColour Black = White

canTake::Piece->(Maybe Piece)->Bool
canTake _ Nothing = False
canTake (Piece _ thisColour _ _) (Just (Piece t colour _ _)) = colour /= thisColour && t /= King

canCheck::Piece->(Maybe Piece)->Bool
canCheck _ Nothing = False
canCheck (Piece _ thisColour _ _) (Just (Piece t colour _ _)) = colour /= thisColour && t == King

createDefaultGame::Game
createDefaultGame = addPiecesToGame [(Piece Queen Black 7 3), (Piece King Black 7 4), (Piece Bishop Black 7 5),
                                    (Piece Bishop Black 7 2), (Piece Queen White 0 3), (Piece King White 0 4),
                                    (Piece Bishop White 0 5), (Piece Bishop White 0 2), (Piece Knight Black 7 1),
                                    (Piece Knight Black 7 6), (Piece Knight White 0 6), (Piece Knight White 0 1),
                                    (Piece Rook Black 7 0), (Piece Rook Black 7 7), (Piece Rook White 0 0),
                                    (Piece Rook White 0 7)] $ fillRow Pawn White 1 7 $ fillRow Pawn Black 6 7 $ emptyGame

pieceString::Maybe Piece->String
pieceString (Just (Piece Pawn c _ _)) = (colourString c) ++ "p"
pieceString (Just (Piece Knight c _ _)) = (colourString c) ++ "k"
pieceString (Just (Piece Bishop c _ _)) = (colourString c) ++ "B"
pieceString (Just (Piece Rook c _ _)) = (colourString c) ++ "R"
pieceString (Just (Piece Queen c _ _)) = (colourString c) ++ "Q"
pieceString (Just (Piece King c _ _)) = (colourString c) ++ "K"
pieceString Nothing = " "

colourString::Colour->String
colourString White = "\ESC[90m"
colourString _ = ""

squareBackground::Int->String
squareBackground i
                    | i `mod` 2 == 0 = "\ESC[43m"
                    | otherwise = ""

showGame::Game->IO ()
showGame g = putStrLn $ gameToString g

rowToString::Game->Int->String
rowToString _ 0 = "   " ++ concat [ (show r) ++ " " | r <- [0..(boardSize-1)]]
rowToString game r = (show rd) ++ " |" ++ rowSquares
                 where
                  rd = r - 1
                  rowMod = rd `mod` 2
                  rowSquares = concat $ do
                                 c <- [0..(boardSize-1)]
                                 let ps = pieceString $ pieceOnSquare rd c game
                                 return ((squareBackground (rowMod+c)) ++ ps ++ "\ESC[0m|")

gameToString::Game->String
gameToString g = unlines $ do
                    r <- reverse [0..boardSize]
                    return (rowToString g r)

fillRow::PieceType->Colour->Int->Int->Game->Game
fillRow pt colour row width game = let
                                addNext 0 acc = (Piece pt colour row 0):acc
                                addNext n acc = addNext (n-1) ((Piece pt colour row n):acc)
                              in
                                addPiecesToGame (addNext width []) game

addMoveToContext::Game->Move->GameContext->GameContext
addMoveToContext game move GameContext {getGame=_,moveListGenerator=mm,checkListGenerator=cm,moveHistory=mh,gameHistory=gh} = GameContext {getGame=game,moveListGenerator=mm,checkListGenerator=cm,moveHistory=move:mh,gameHistory=game:gh}

movePieceWithMove::Move->GameContext->GameContext
movePieceWithMove m gameC = let
                              game = doMove m $ getGame gameC
                            in
                              addMoveToContext game m gameC

gameIsDraw::[Game]->Bool
gameIsDraw gameList = headRepeatedNTimes 3 gameList

headRepeatedNTimes::(Eq a)=>Int->[a]->Bool
headRepeatedNTimes _ [] = False
headRepeatedNTimes n l = (length . filter (==h) $ l) >= n
                        where
                          h = head l

everyNth::(Eq a)=>Int->[a]->[a]
everyNth n l = let
                 nth _ [] acc = reverse acc
                 nth i (x:xs) acc
                               | i == n || i == 0 = nth 1 xs (x:acc)
                               | otherwise = nth (i+1) xs acc
               in
                 nth 0 l []

lastNAreRepeats::(Eq a)=>Int->[a]->Bool
lastNAreRepeats _ [] = False
lastNAreRepeats n l = length nthList >= n && all (==(head l)) nthList
                     where
                      nthList = take n . everyNth 4 $ l
