module PlayerGen (
    genParamList,
    allOrderedPairs,
    allUnorderedPairs,
    mostCommonElement,
    nMostCommonElements,
    createNewGeneration,
    ParamList
) where

import System.Random
import Control.Monad
import Data.List
import qualified Data.Map as Map

type ParamList = [[Int]]

genParamList::IO [[Int]]
genParamList = genParameters 6 48

genParameters::Int->Int->IO [[Int]]
genParameters numPiece numParam = do
                                    liftM (genListOfLists numPiece numParam) params
                                  where
                                    params = do
                                                g <- newStdGen
                                                return (randomRs (0,5) g)

splitS::[a]->[[a]]->Int->([a],[[a]])
splitS s e n = (s2, s1:e)
            where
              (s1,s2) = splitAt n s

genListOfLists::Int->Int->[a]->[[a]]
genListOfLists nl ni source = let
                                gen 0 (_,acc) = acc
                                gen n (s,acc) = gen (n-1) (splitS s acc ni)
                              in
                                gen nl (source,[])

allUnorderedPairs::[a]->[(a,a)]
allUnorderedPairs l = pairs l l []
            where
              pairs [] [] acc = acc
              pairs (x:xs) (_:ys) acc = pairs xs ys $ (zip (replicate (length ys) x) ys) ++ acc
              pairs _ _ _ = error "Mismatched lists"

allOrderedPairs::(Eq a) => [a]->[(a,a)]
allOrderedPairs l = [(x,y) | x <- l, y <- l, x /= y]

accumMap::(Ord a)=>a->(Map.Map a Int)->(Map.Map a Int)
accumMap x m = Map.insertWith (\new old -> new + old) x 1 m

occurMap::(Ord a)=>[a]->(Map.Map a Int)
occurMap l = foldl (\m x -> accumMap x m) Map.empty l

mostCommonElement::(Ord a) => [a]->a
mostCommonElement l = maxVal . occurMap  $ l
                   where
                     maxVal m = fst $ Map.foldlWithKey (\(bestKey,bestVal) k a -> if (a > bestVal) then (k,a) else (bestKey,bestVal)) (Map.findMin m) m

nMostCommonElements::(Ord a) => Int->[a]->[a]
nMostCommonElements n l  = map fst . take n $ sortByOccur l
                            where
                           sortByOccur lt = reverse . sortOn (snd) $ Map.assocs . occurMap $ lt

replaceNth::[a]->Int->(a->a)->[a]
replaceNth l n f = let
                     repl [] _ acc = reverse acc
                     repl (x:xs) 0 acc = reverse ((f x):acc) ++ xs
                     repl (x:xs) c acc = repl xs (c-1) (x:acc)
                   in
                     repl l n []

modifyParamList::ParamList->IO ParamList
modifyParamList l = do
                        let listLen = length l
                        let subListLen = length (head l)
                        ln <- getStdRandom (randomR (0,listLen-1))
                        sln <- getStdRandom (randomR (0,subListLen-1))
                        mult <- getStdRandom (randomR (0.0,2.0))::IO Double
                        add <- getStdRandom (randomR (-2,2))::IO Int
                        return (replaceNth l ln (\sl -> replaceNth sl sln (\n -> abs $ add + round (mult*(fromIntegral n)))))

makeNModifications::Int->ParamList->IO ParamList
makeNModifications 0 l = return l
makeNModifications n l = do
                            newL <- modifyParamList l
                            makeNModifications (n-1) newL

splice::[a]->[a]->Int->[a]
splice l1 l2 n = (take n l1) ++ (drop n l2)

spliceLists::[a]->[a]->IO [a]
spliceLists l1 l2 = do
                        let listLen = length l1
                        ln <- getStdRandom (randomR (0,listLen-1))
                        d <- getStdRandom (randomR (0::Int,1::Int))
                        case d of
                          0 -> return (splice l1 l2 ln)
                          1 -> return (splice l2 l1 ln)

spliceListPair::(ParamList,ParamList)->IO ParamList
spliceListPair (l1,l2) = spliceLists l1 l2

multiplyList::[a]->Int->[a]
multiplyList l n = concat $ replicate n l

spliceGeneration::[ParamList]->IO [ParamList]
spliceGeneration l = do
                        let pairs = allOrderedPairs l
                        mapM spliceListPair (multiplyList pairs 2)

createNewGeneration::[ParamList]->Int->Int->IO [ParamList]
createNewGeneration parents nChanges nChildren = let
                                                  create::Int->[ParamList]->ParamList->IO [ParamList]
                                                  create 0 acc _ = return acc
                                                  create n acc parent = do
                                                                    c <- getStdRandom(randomR ((nChanges `div` 2,nChanges*2)))
                                                                    child <- makeNModifications c parent
                                                                    create (n-1) (child:acc) parent
                                                 in
                                                    do
                                                       generation <- (mapM (create nChildren []) parents)
                                                       let initialGen = (parents ++ (concat generation))
                                                       spliced <- spliceGeneration parents
                                                       return (initialGen ++ spliced)
