module Trie (
    Trie(..),
    find,
    findLongestMatch,
    insert,
    emptyTrie,
    getFullMatches,
    fromList,
    fromFile
) where

import Control.Monad

data Trie a b = Node (Maybe a) [(b,Trie a b)] deriving (Show)

emptyTrie::Trie a b
emptyTrie = Node Nothing []

insert::(Eq b)=>[b]->a->(Trie a b)->(Trie a b)
insert [] v (Node Nothing l) = Node (Just v) l
insert [] v (Node _ l) = Node (Just v) l
insert (x:xs) v (Node cv l) = let
                                (before, after) = break (\(xe,_)->xe == x) l
                             in
                               case after of
                                [] -> Node cv ((x,(insert xs v emptyTrie)):before)
                                (_,ty):ys -> Node cv (before ++ (x, (insert xs v ty)):ys)

find::(Eq b)=>[b]->(Trie a b)->Maybe a
find [] (Node v _) = v
find (x:xs) (Node _ l) = lookup x l >>= find xs

maybePair::[a]->Maybe b->Maybe ([a],b)
maybePair s d = d >>= (\x -> Just (reverse s,x))

findMatch::(Eq b)=>[b]->Maybe ([b],a)->(Trie a b)->[b]->Maybe ([b],a)
findMatch [] d (Node Nothing _) _ = d
findMatch [] _ (Node v _ ) s = maybePair s v
findMatch (x:xs) d (Node v l) s = let
                                  n = lookup x l
                                in
                                  case n of
                                    Nothing -> case v of
                                                  Nothing -> d
                                                  _ -> maybePair s v
                                    (Just nd) -> case v of
                                                  Nothing -> findMatch xs d nd (x:s)
                                                  _ -> findMatch xs (maybePair s v) nd (x:s)

findLongestMatch::(Eq b)=>[b]->Trie a b->Maybe ([b], a)
findLongestMatch l t = findMatch l Nothing t []

findNodeMatch::(Eq b)=>[b]->(Trie a b)->Maybe (Trie a b)
findNodeMatch [] n = Just n
findNodeMatch (x:xs) (Node _ l) = lookup x l >>= findNodeMatch xs


getSuffixes::Trie a b->[[b]]
getSuffixes n = let
                  get::(Trie a b)->[[b]]->[b]->[[b]]
                  get (Node (Just _) []) accList acc = acc:accList
                  get (Node Nothing []) accList _ = accList
                  get (Node v xs) accList acc = let
                                                  nextAccList = concat [get t accList (y:acc) | (y,t) <- xs]
                                                in
                                                  case v of
                                                         Just _ -> acc:nextAccList
                                                         Nothing -> nextAccList
                 in
                    map reverse $ get n [] []

getFullMatches::(Eq b)=>[b]->Trie a b->[[b]]
getFullMatches l t = case (liftM (getSuffixes) $ findNodeMatch l t) of
                    Just v -> map (l++) v
                    Nothing -> []

fromList::(Eq b)=>[[b]]->a->(Trie a b)
fromList w v = foldl (\n i -> insert i v n) emptyTrie w

fromFile::String->IO (Trie Int Char)
fromFile path = do
                    contents <- readFile path
                    return (fromList (lines contents) 1)

