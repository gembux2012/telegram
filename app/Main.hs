{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           App
import           Control.Error                     (ExceptT, runExceptT)
import           Control.Monad                     ()
import           Control.Monad.Catch.Pure          ()
import           Control.Monad.Reader              (ReaderT, asks, runReaderT,
                                                    unless)
import           Control.Monad.State               (StateT, evalStateT, get,
                                                    modify)
import           Data.Aeson                        (encode)
import qualified Data.ByteString.Char8             as BS8
import qualified Data.ByteString.Lazy              as LBS
import           Data.Has                          (getter)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe)
import qualified Data.Text                         as T
import           Error
import           Logger.Class
import           Text.Read                         ()
--import Logger.Types (Config(..), TelegramOpts(..), defaultLogOpts, Priority(..) )
import           Control.Concurrent                (forkIO)
import           GHC.Base                          (error)
import           Prelude                           hiding (FilePath, error, log)
import           System.Directory.Internal.Prelude (exitFailure, hFlush, stdout)
import           Turtle                            (FilePath, encodeString)
--import Config
import           System.Exit                       (exitSuccess)
--import Types
import           Config.Config
import           Config.Types
import           Control.Concurrent.Async          (race_)
import           Control.Monad.Cont                (forever, when)
import           Logger.Types
import           Streams                           (initLog, msgLog, stopLog)
import           Telegram.Api
import           Turtle                            (printf)
import           Turtle.Options                    (options)
import           Types
import Data.ByteString.Char8 (hPutStr)


main =  startBot

startBot :: IO ()
startBot  = do
  (Settings api config_path) <- options description settingsP

  let list_user = Map.empty

  conf <- readConfig  config_path
  case conf of
    Left  error -> do
     log <-initLog defaultLogOpts
     msgLog log (fromLoggable INFO <> T.pack " bot abnormal termination ")
     msgLog log (fromLoggable ERROR <> T.pack " file : " <> T.pack error )
     stopLog log
    Right config -> do
      log <-initLog (logOpts config)
      let logger = Logger $ msgLog log
      let api' = telegram

      race_ ( runBot api' logger (telegramOpts config) list_user log) (do {
                                                                              forever $ do
                                                                                hPutStr "bot:>"
                                                                                hFlush stdout
                                                                                arg <- getLine
                                                                                putStrLn "kilkujlk"
                                                                                case arg of
                                                                                  "stop" -> do
                                                                                    msgLog log (fromLoggable INFO <> T.pack " bot stopped ")
                                                                                    stopLog log
                                                                                    exitSuccess
                                                                                  _ -> msgLog log (fromLoggable INFO <> T.pack " unknown commnd " <> T.pack arg)
                                                                                return()
                                                                              })



runBot api logger config  list_user log =
 do
     res <- evalStateT (runReaderT (runExceptT  api ) (logger, config)) list_user
     case res of
       Left (BotError error) -> do
        msgLog log (fromLoggable ERROR <> T.pack " " <> fromLoggable error)
        stopLog log
        exitSuccess
       Right _ -> msgLog log (fromLoggable INFO <> T.pack " " <> fromLoggable " OK ")







