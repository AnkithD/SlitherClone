module Main where

import Network.Socket
import Control.Concurrent
import System.IO

main :: IO()
main = do
    putStrLn "Started Server..."
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 7479 iNADDR_ANY
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do
    connection <- accept sock
    putStrLn "Accepted Client's connection"
    forkIO $ connHandler connection
    mainLoop sock

connHandler :: (Socket, SockAddr) -> IO()
connHandler (sock, _) = do
    putStrLn "Starting Handler"
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    hPutStrLn handle "Hello Client!"
    putStrLn "Waiting for Input"
    msg <- hGetLine handle
    putStrLn "Got Input: "
    putStrLn msg
    hClose handle