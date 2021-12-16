{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tmp where

import App
import Control.Error (ExceptT, runExceptT)
import Control.Monad ()
import Control.Monad.Reader (ReaderT, runReaderT, unless, asks)
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Error
import Data.Aeson (encode)
import  qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Control.Monad.State ( evalStateT, modify, get, StateT, StateT, StateT)
import Text.Read ()
import Logger.Class
import Control.Monad.Catch.Pure ()
import qualified Data.Text as T
--import Logger.Types (Config(..), TelegramOpts(..), defaultLogOpts, Priority(..) )
import Turtle (FilePath, encodeString)
import Prelude hiding (FilePath,error,log)
import Control.Concurrent (forkIO)
import System.Directory.Internal.Prelude (exitFailure)
import GHC.Base (error)
--import Config
import System.Exit (exitSuccess)
--import Types
import Streams (initLog, msgLog, stopLog)
import Turtle.Options (options)
import Control.Monad.Cont (when, forever)
import Turtle (printf)
import Types
import Config.Config
import Config.Types
import Logger.Types
import Telegram.Api


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
      when ( T.pack "t" /=  api) do
        msgLog log (fromLoggable INFO <> T.pack " unknown key --" <> api <> T.pack " will be launched bot for telegramm")
        forkIO $ runBot telegram logger (telegramOpts config) list_user log
        return()
      
      forkIO $ runBot telegram logger (telegramOpts config) list_user log
      forever $ do
        arg <- getLine
        case arg of
          "stop" -> do
            msgLog log (fromLoggable INFO <> T.pack " bot stopped ")
            stopLog log
            exitSuccess
          _ -> msgLog log (fromLoggable INFO <> T.pack " unknown commnd " <> T.pack arg) 
          
      stopLog log

 
  
runBot api logger config  list_user log =
 do
     res <- evalStateT (runReaderT (runExceptT  api ) (logger, config)) list_user
     case res of
       Left (BotError error) ->  msgLog log (fromLoggable ERROR <> T.pack " " <> fromLoggable error)
       Right _ -> msgLog log (fromLoggable INFO <> T.pack " " <> fromLoggable " OK ")









