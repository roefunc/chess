module ComputerPlayer (
    computerPlayer,
    genRandomComputerPlayer,
    compareParamLists,
    genRandomCoeffLists,
    selectNBestFromList,
    doNGenerationsRandom,
    doNGenerations,
    createComputerPlayer
) where

import Chess
import ChessUtils
import ChessTypes
import Control.Parallel.Strategies
import Control.Applicative
import Game
import PlayerGen
import qualified Data.Map.Strict as Map

type EvalFunc = Piece->Int
type ScoreFuncMap = Map.Map PieceType [EvalFunc]

favourColumn::Int->EvalFunc
favourColumn fc (Piece _ _ _ col) | fc == col = 1
                                  | otherwise = 0

favourRow::Int->EvalFunc
favourRow fr (Piece _ _ row _) | fr == row = 1
                               | otherwise = 0

invertOnColour::Colour->EvalFunc->EvalFunc
invertOnColour colour f p@(Piece _ pieceColour _ _) | pieceColour == colour = f p
                                                    | otherwise = (-1)*(f p)

favourColumnByColour::Colour->Int->EvalFunc
favourColumnByColour colour fc = invertOnColour colour (favourColumn fc)

favourRowByColour::Colour->Int->EvalFunc
favourRowByColour colour fr = invertOnColour colour (favourRow fr)

-- generate a list of scoring functions for all rows or columns
forBoardWidth::(Int->b->c)->[b->c]
forBoardWidth f = [f s | s <- [0..7]]

multCoeff::EvalFunc->Int->EvalFunc
multCoeff f c p = c * (f p)

-- use the passed in list of coefficients to create a list of
-- scoring functions for each Piece
evalFuncs::[Int]->[EvalFunc]
evalFuncs l = getZipList $ ZipList (map multCoeff $ concatMap forBoardWidth funcs) <*> ZipList l
             where
                funcs = [favourColumn, favourRow, (favourColumnByColour White), (favourColumnByColour Black), (favourRowByColour White), (favourRowByColour Black)]

-- create a map between PieceType and the list of scoring functions to be used
genScoreFuncMap::ParamList->ScoreFuncMap
genScoreFuncMap coeffs = let
                           gen [] _ acc = acc
                           gen (pt:ptrest) (cs:csrest) acc = gen ptrest csrest (Map.insert pt (evalFuncs cs) acc)
                           gen _ _ _ = error "Mismatched lists"
                         in
                           gen [Pawn,Rook,Knight,Bishop,Queen,King] coeffs Map.empty

genRandomScoreFuncMap::IO ScoreFuncMap
genRandomScoreFuncMap = do
                          paramList <- genParamList
                          return (genScoreFuncMap  paramList)

genRandomCoeffLists::Int->IO [ParamList]
genRandomCoeffLists n = let
                          gen::Int->IO [ParamList]->IO [ParamList]
                          gen 0 acc = acc
                          gen c acc = do
                                        a <- acc
                                        gen (c-1) (fmap (:a)  genParamList)
                        in
                          gen n (return [])

-- The static value of each piece type
pieceValue::Piece->Int
pieceValue (Piece Pawn _ _ _) = 100
pieceValue (Piece Knight _ _ _) = 300
pieceValue (Piece Bishop _ _ _) = 300
pieceValue (Piece Rook _ _ _) = 500
pieceValue (Piece Queen _ _ _) = 900
pieceValue (Piece King _ _ _) = 0

scoreMult::Colour->Int
scoreMult Black = -1
scoreMult White = 1

getFuncs::Piece->ScoreFuncMap->[EvalFunc]
getFuncs (Piece pt _ _ _) fm = fm Map.! pt

paramScore::Piece->ScoreFuncMap->Int
paramScore p fm = foldl (+) 0 $ map ($p) scoreFuncs
                 where
                    scoreFuncs = getFuncs p fm

-- Add the static and positional scores for a piece and +/- for the colour
scorePiece::Piece->ScoreFuncMap->Int
scorePiece p@(Piece _ c _ _) fm = (scoreMult c) * ((pieceValue p) + (paramScore p fm))

-- fold scoring functions over the game to produce the overall static score
sumValues::Game->ScoreFuncMap->Int
sumValues game fm = foldGame (\s p -> s+(scorePiece p fm)) 0 game

compForColour::(Ord a)=>Colour->(a->a->Bool)
compForColour Black = (<)
compForColour White = (>)

scoreGame::GameContext->Colour->Int->ScoreFuncMap->Int
scoreGame gameC _ 0 fm = sumValues (getGame gameC) fm
scoreGame gameC colour depth fm = if (gameIsDraw (gameHistory gameC)) then 0 else calcScore
                                where
                                  noMoveScore = if (isCheck gameC (oppColour colour)) then
                                                (scoreMult colour) * 10000
                                              else
                                                (scoreMult colour) * 1000
                                  calcScore = case (chooseMove (depth-1) fm (oppColour colour) gameC) of
                                                Just (_, score, _) -> score + ((scoreMult colour)*depth)
                                                _ -> noMoveScore

-- Return the better move for the passed in colour
betterMove::Colour->(Move, Int, Game)->(Move, Int, Game)->(Move, Int, Game)
betterMove colour l@(_, ls, _) r@(_, rs, _) = if (ls `comp` rs) then l else r
                                 where
                                    comp = compForColour colour

--showMoves::[(Move,Int,Game)]->String
--showMoves ((m,s,_):rest) = "Move: " ++ (show m) ++ " Score: " ++ (show s) ++ "\n" ++ (showMoves rest)
--showMoves  _ = ""

parStrat::(NFData a)=>Int->(Strategy [a])
parStrat n
            | n >= 1 = parList rdeepseq
            | otherwise = parList r0

chooseMove::Int->ScoreFuncMap->Colour->GameContext->Maybe (Move, Int, Game)
chooseMove depth fm colour gameC = let
                                 possMovesAndGames = allPossibleGames gameC colour
                                 moveScores = (map (\(m,g) -> (m, (scoreGame g colour depth fm), (getGame g))) $ possMovesAndGames) `using` (parStrat depth)
                               in
                                 if (null possMovesAndGames) then
                                   Nothing
                                 else
                                   Just (foldl (betterMove colour) (head moveScores) moveScores)

compPlayer::ScoreFuncMap->Int->Colour->GameContext->Maybe Move
compPlayer fm depth colour gameC = (chooseMove depth fm colour gameC) >>= (\(m,_,_) -> Just m)

computerPlayer::ScoreFuncMap->Int->Colour->GameContext->IO (Maybe Move)
computerPlayer fm depth colour gameC = return (compPlayer fm depth colour gameC)

genRandomComputerPlayer::Int->Colour->IO (GameContext->IO (Maybe Move))
genRandomComputerPlayer depth colour = do
                        m <- genRandomScoreFuncMap
                        return (computerPlayer m depth colour)

createComputerPlayer::ParamList->Int->Colour->GameContext->IO (Maybe Move)
createComputerPlayer params depth colour gameC = computerPlayer (genScoreFuncMap params) depth colour gameC

compareParamLists::ParamList->ParamList->Int->IO ParamList
compareParamLists m1 m2 depth = let
                                  player1 = computerPlayer (genScoreFuncMap m1) depth White
                                  player2 = computerPlayer (genScoreFuncMap m2) depth Black
                                in
                                  do
                                    c <- findBetterPlayer createDefaultGameContext player1 player2 White 150
                                    putStrLn ((show c) ++ " wins!")
                                    case c of
                                      White ->return m1
                                      _ -> return m2

-- Generate all possible games (both sides playing black and white)
-- and return the ParamList that won the most games
selectNBestFromList::Int->[ParamList]->Int->IO [ParamList]
selectNBestFromList _ [x] _ = return [x]
selectNBestFromList n l depth = let
                                pairs = allOrderedPairs l
                                doBrackets = mapM (\(m1,m2) -> compareParamLists m1 m2 depth) pairs
                             in
                                do
                                  putStrLn ("Starting " ++ (show (length pairs)) ++ " games")
                                  b <- doBrackets
                                  return (nMostCommonElements n b)

doGeneration::[ParamList]->Int->Int->Int->IO [ParamList]
doGeneration pl changes size depth = do
                         generation <- createNewGeneration pl changes size
                         best <- selectNBestFromList 2 generation depth
                         return best

doNGenerations::Int->Int->Int->Int->[ParamList]->IO [ParamList]
doNGenerations 0 _ _ _ pl = return pl
doNGenerations gens changes size depth pl = do
                                                putStrLn ("Starting generation " ++ (show gens))
                                                putStrLn "Current best"
                                                putStrLn (show pl)
                                                best <- doGeneration pl changes size depth
                                                doNGenerations (gens-1) changes size depth best

doNGenerationsRandom::Int->Int->Int->Int->IO [ParamList]
doNGenerationsRandom gens changes size depth = do
                                                firstList <- genParamList
                                                doNGenerations gens changes size depth [firstList]
