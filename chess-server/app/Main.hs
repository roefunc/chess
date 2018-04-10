import HttpServer
import FileHandler
import System.Environment
import Trie
import Control.Monad
import SuggestionHandler
import ChessHandler

getUrlHandlerSelector::IO (String->Maybe (String, Handler))
getUrlHandlerSelector = liftM (\t s -> findLongestMatch s t) getHandlers

getHandlers::IO (Trie Handler Char)
getHandlers = do
                suggestTrie <- fromFile "/usr/share/dict/words"
                return (insert "/chess/" chessHandler $ insert "/suggest/" (suggestionHandler suggestTrie) $ insert "/" fileHandler emptyTrie)

main::IO()
main = do
            args <- getArgs
            handlerSelector <- getUrlHandlerSelector
            startHttpServer 8000 ( Config { rootDir=(head args), notFoundPage="not_found.html",urlHandler=handlerSelector} )

