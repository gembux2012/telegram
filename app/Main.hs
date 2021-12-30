{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Error                     (ExceptT, runExceptT, isJust)
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
import           Control.Concurrent                (forkIO)
import           GHC.Base                          (error)
import           Prelude                           hiding (FilePath, error, log)
import           System.Directory.Internal.Prelude (exitFailure, hFlush, stdout)
import           System.Exit                       (exitSuccess)
import           Config.Config
import           Config.Types
import           Control.Concurrent.Async          (race_)
import           Control.Monad.Cont                (forever, when)
import           Data.ByteString.Char8             (hPutStr)
import           Logger.Types
import           Telegram.Api                      (telegram)
import           Telegram.Types                    (TelegramOpts)
import           Turtle                            (printf)
import           Turtle.Options                    (options)
import           Types
import App (initLog, msgLog, stopLog)



main =  initBot >>= \set  -> runBot set

initBot = do
  (Settings msgndgr config_path) <- options description settingsP
  config <- readConfig config_path
  case config of
    Left error -> do
      log <- initLog defaultLogOpts
      msgLog log INFO (fromLoggable INFO <> " bot abnormal termination, see error log ")
      msgLog log ERROR (fromLoggable ERROR <> " file : " <> T.pack error)
      stopLog log
      exitFailure
    Right config -> do
      log <- initLog (logOpts config)
      case switchMessenger (T.unpack <$> msgndgr) config of
        Right api -> do
          msgLog log INFO (fromLoggable INFO <> T.pack (snd api))
          return $ SettingsB Map.empty config (Logger $ msgLog log) log (fst api)
        Left er -> do
          msgLog log INFO (fromLoggable INFO <> " bot abnormal termination, see error log ")
          msgLog log ERROR (fromLoggable ERROR <> T.pack er)
          stopLog log
          exitFailure

runBot settings  =
      race_
        (do res <- evalStateT (runReaderT (runExceptT  (api settings )) (logger settings, config settings)) (list_user settings)
            case res of
              Left (BotError error) -> do
               msgLog (log settings) ERROR (fromLoggable ERROR <> "  " <> T.pack error)
               stopLog (log settings)
               exitSuccess
              Right _ -> msgLog (log settings) INFO (fromLoggable INFO <> " " <> " OK ")
              )
       -- | to do I don’t know how else to stop it
        (forever $ do
           arg <- getLine
           case arg of
             "stop" -> do
               msgLog (log settings) INFO (fromLoggable INFO <> T.pack " bot stopped from console ")
               stopLog (log settings)
               exitSuccess
             _ -> msgLog (log settings) INFO (fromLoggable INFO <> T.pack " unknown commnd " <> T.pack arg)
           return ())


switchMessenger val  config =
 case val of
  Just "t" -> default_ "will be launched telegram"
  Just k  -> default_ $ " key '-" <> k <> "' not found, will be launched telegram"
 where
  default_ msg | isJust (telegramOpts config) = Right (telegram, msg)
               | otherwise = Left " section 'telegramOpts' not found in config file"

