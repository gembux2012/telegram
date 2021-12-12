module Streams where

import Control.Concurrent.Lifted (putMVar,takeMVar,newEmptyMVar,MVar)
import Control.Concurrent (forkIO)

import Logger.Types
import Logger.App (printLog)
import Data.Text (Text)


newtype Stream = Stream (MVar LogCommand)
data LogCommand = Message Text | Stop (MVar ())  

initStdOutStream :: Config -> IO Stream
initStdOutStream conf = do
  m <- newEmptyMVar
  let stream = Stream m 
  _ <- forkIO  (stdOutStream conf stream)
  return stream    
  
initStdInStream ::  IO Stream
initStdInStream  = do
  m <- newEmptyMVar
  let stream = Stream m 
  _ <- forkIO  (stdInStream  stream)
  return stream    
    

msgStream :: Stream -> Text -> IO ()
msgStream (Stream m) s = putMVar m (Message s)

stopStream :: Stream -> IO ()
stopStream (Stream m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s 
 
stdOutStream ::Config -> Stream -> IO ()
stdOutStream conf (Stream m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          printLog (logOpts conf) msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()
          
stdInStream :: Stream -> IO ()
stdInStream  (Stream m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          printLog (logOpts conf) msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()          