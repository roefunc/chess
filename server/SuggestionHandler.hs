module SuggestionHandler (
    suggestionHandler
) where

import HttpTypes
import Trie
import Data.Int
import System.Posix.Types
import qualified Data.ByteString.Lazy as B

suggestionHandler::(Trie Int Char)->Handler
suggestionHandler t = Handler { sendData=(sendSuggestion t) }


sendSuggestion::(Trie Int Char)->Sender->Config->HttpCommand->String->IO Int64
sendSuggestion t f _ _ name = do
                              putStrLn ("Word:" ++ name)
                              let suggs = packStr $ unlines $ getFullMatches name t
                              let len = COff (fromIntegral (B.length suggs)::Int64)
                              f (suggHeader name len) suggs
                              where
                                suggHeader word resultLen = OK "text/plain" word resultLen
