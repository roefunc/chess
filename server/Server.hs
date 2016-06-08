module Server (
    start,
    AcceptFn
) where

import Network.Socket
import Data.Int
import Control.Concurrent

type AcceptFn = (Socket,SockAddr)->IO Int64

start::AcceptFn->PortNumber->IO()
start runConn port = do
                sock <- createSocket port
                mainLoop sock runConn

createSocket::PortNumber->IO Socket
createSocket port = do
                sock <- socket AF_INET Stream 0
                setSocketOption sock ReuseAddr 1
                bindSocket sock (SockAddrInet port iNADDR_ANY)
                listen sock 20 
                return sock

mainLoop::Socket->AcceptFn->IO ()
mainLoop sock runConn = do
                    conn <- accept sock
                    forkIO $ do
                      runConn conn
                      let (clientSock, _) = conn
                      sClose clientSock
                      return ()
                    mainLoop sock runConn

