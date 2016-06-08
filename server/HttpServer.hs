module HttpServer (
    startHttpServer,
    Header(..),
    Handler(..),
    Config(..),
) where

import Server
import Http
import HttpTypes

import Network.Socket hiding (sendTo, recvFrom)
import qualified Network.Socket.ByteString.Lazy as NB
import qualified Data.ByteString.Lazy as B
import Data.Int
import System.IO.Error

startHttpServer::PortNumber->Config->IO()
startHttpServer port config = do
                            start (serveLoop config) port

fileHeaderString::Header->String
fileHeaderString (OK t _ s) = "HTTP/1.0 200 OK\n" ++ "Content-Type: " ++ t ++ "\r\nContent-Length: " ++ (show s) ++ "\r\nAccess-Control-Allow-Origin: * \r\n\r\n"
fileHeaderString (NotFound _) = "HTTP/1.0 404 Not Found\r\n\r\n"

getIntFromHeader::String->[(String,String)]->Int
getIntFromHeader _ [] = 0;
getIntFromHeader s ((header,value):xs)
                                  | s == header = read value :: Int
                                  | otherwise = getIntFromHeader s xs

fillHttpCommand::HttpCommand->[(String,String)]->Socket->IO HttpCommand
fillHttpCommand (Post{url=u,content=_}) headers sock = do
                                          let contentLength = getIntFromHeader "Content-Length" headers
                                          newContent <- recv sock contentLength
                                          putStrLn ("Read " ++ newContent ++ " from POST")
                                          return Post {url=u,content=newContent}
fillHttpCommand c _ _ = return c

serveLoop::Config->(Socket,SockAddr)->IO Int64
serveLoop config (sock,_) = do
                        msg <- recv sock 4096
                        let (httpCommand, httpHeaders) = parseHttpRequest msg
                        filledHttpCommand <- fillHttpCommand httpCommand httpHeaders sock
                        bytes <- handleHttpRequest sock config filledHttpCommand httpHeaders
                        sClose sock
                        return bytes

sendHeaders::(B.ByteString->IO Int64)->Header->IO Int64
sendHeaders f header = do
                         hbytes <- f (packStr (fileHeaderString header))
                         return hbytes

fullSender::(B.ByteString->IO Int64)->Header->B.ByteString->IO Int64
fullSender f h s = do
                    putStrLn (show h)
                    hBytes <- sendHeaders f h
                    fileBytes <- f s
                    return (hBytes + fileBytes)

headerSender::(B.ByteString->IO Int64)->Header->B.ByteString->IO Int64
headerSender f h _ = do
                    putStrLn (show h)
                    hBytes <- sendHeaders f h
                    return hBytes



doCommand::Socket->Config->HttpCommand->[(String,String)]->Maybe (IO Int64)
doCommand sock config command headers = do
                                          (sender, uri) <- getCommand command
                                          (prefix,handler) <- (urlHandler config) uri
                                          return ((sendData handler) (sender (safeSend sock)) config command $ drop (length prefix) uri)
                                        where
                                          getCommand (Get { url = u} ) = Just (fullSender,u)
                                          getCommand (Post { url = u} ) = Just (fullSender,u)
                                          getCommand (Head { url = u} ) = Just (headerSender,u)
                                          getCommand _ = Nothing
                                          safeSend netSock s = catchIOError (NB.send netSock s) (\e -> do
                                                                                             putStrLn (show e)
                                                                                             return 0)

handleHttpRequest::Socket->Config->HttpCommand->[(String,String)]->IO Int64
handleHttpRequest sock config command headers =  case (doCommand sock config command headers) of
                                                    Just i -> i
                                                    Nothing -> return 0
