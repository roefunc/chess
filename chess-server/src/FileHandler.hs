module FileHandler (
    fileHandler
) where

import HttpTypes
import qualified Data.ByteString.Lazy as B
import System.Posix.Files
import System.IO.Error
import Data.Int

fileHandler::Handler
fileHandler = Handler { sendData=sendFile }

headerPath::Header->String
headerPath (OK _ path _) = path
headerPath (NotFound path) = path

splitLast::(Eq a)=>[a]->a->[a]
splitLast l p = let
                  doSplit [] acc = acc
                  doSplit (x:xs) acc = if (x == p) then doSplit xs xs else doSplit xs acc
                in
                  doSplit l []

fileType::String->String
fileType name = case (splitLast name '.') of
                  "js" -> "application/javascript"
                  "png" -> "image/png"
                  "jpg" -> "image/jpg"
                  "html" -> "text/html"
                  "txt" -> "text/plain"
                  _ -> "text/plain"

fileHeader::Config->String->IO Header
fileHeader config name = do
                    let path = (rootDir config) ++ name
                    exists <- fileExist path
                    case exists of
                      True -> do
                        status <- getFileStatus path
                        if (isDirectory status) then
                          fileHeader config (name ++ "index.html")
                        else
                          return (OK (fileType name) path (fileSize status))
                      False -> return (NotFound (rootDir config ++ (notFoundPage config)))

sendFile::Sender->Config->HttpCommand->String->IO Int64
sendFile f config _ name = catchIOError (do
                                        putStrLn ("FileHandler: " ++ name)
                                        header <- fileHeader config name
                                        contents <- B.readFile (headerPath header)
                                        f header contents)
                                      (\_ -> do
                                         putStrLn ("Missing file " ++ name)
                                         return 0)


