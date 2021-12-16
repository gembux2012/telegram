module Streams where

import           Control.Concurrent        (forkIO)
import           Control.Concurrent.Lifted (MVar, newEmptyMVar, putMVar,
                                            takeMVar)

import           Data.Text                 (Text)
import           Logger.App                (printLog)
import           Logger.Types


newtype Stream = Stream (MVar LogCommand)
data LogCommand = Message Text | Stop (MVar ())

initLog :: LogOpts -> IO Stream
initLog conf = do
  m <- newEmptyMVar
  let stream = Stream m
  _ <- forkIO  (logger conf stream)
  return stream



msgLog :: Stream -> Text -> IO ()
msgLog (Stream m) s = putMVar m (Message s)

stopLog :: Stream -> IO ()
stopLog (Stream m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

logger ::LogOpts -> Stream -> IO ()
logger logOpts (Stream m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          printLog logOpts  msg
          loop
        Stop s -> do
        --  putStrLn "logger: stop"
          putMVar s ()

