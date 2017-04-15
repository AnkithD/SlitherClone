module Main where

import Network.Socket
import Control.Concurrent
import System.IO

splitString :: Eq a => a -> [a] -> [[a]]
splitString x [] = []
splitString x xs = p : splitString x (drop 1 q) where (p,q) = span (/= x) xs

data Shape = Rectangle Float Float Float Float
intersect :: Shape -> Shape -> Bool
intersect (Rectangle x1 y1 x2 y2) (Rectangle x3 y3 x4 y4)
       | x1 > x4 = False
       | x2 < x3 = False
       | y1 > y4 = False
       | y2 < y3 = False
       | otherwise = True

main :: IO()
main = do
    putStrLn "Started Server and Listing on Port: 7479..."
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 7479 iNADDR_ANY
    listen sock 10
    mainLoop sock

mainLoop :: Socket -> IO()
mainLoop sock = do
    connection1 <- accept sock
    putStrLn "Accepted First Clients connection"
    connection2 <- accept sock
    putStrLn "Accepted Second Clients connection"
    connHandler connection1 connection2
    mainLoop sock

connHandler :: (Socket, SockAddr) -> (Socket, SockAddr) -> IO()
connHandler (sock1, _) (sock2, _) = do
    putStrLn "Starting Handler"
    handle1 <- socketToHandle sock1 ReadWriteMode
    handle2 <- socketToHandle sock2 ReadWriteMode
    hSetBuffering handle1 LineBuffering
    hSetBuffering handle2 LineBuffering
    -- hPutStrLn handle "Hello Client!"
    handleLoop handle1 handle2

handleLoop :: Handle -> Handle -> IO()
handleLoop handle1 handle2 = do
    --putStrLn "Waiting for Input 1"
    msg1 <- hGetLine handle1
    --putStrLn "Got Input 1: "
    --putStrLn msg1
    --putStrLn "Waiting for Input2"
    msg2 <- hGetLine handle2
    --putStrLn "Got Input 2: "
    --putStrLn msg2

    if msg1 == "QUIT" then do
        hClose handle1
    else if msg2 == "QUIT" then do
        hClose handle2
    else do
        hPutStrLn handle1 msg2
        hPutStrLn handle2 msg1
        handleLoop handle1 handle2