{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Config.Types              (Config)
import           Control.Concurrent.Lifted (MVar)
import           Control.Error             (ExceptT)
import           Control.Monad.Reader      (ReaderT)
import           Control.Monad.State.Lazy  (StateT)
import qualified Data.Map                  as Map
import           Error                     (BotError)
import           Logger.Class              (Logger)
import           Logger.Types              (Priority (..))
import           Prelude                   hiding (FilePath)
import           Telegram.Types            (TelegramOpts)
import           Turtle


newtype Stream = Stream (MVar LogCommand)
data LogCommand = Message Priority Text | Stop (MVar ())



--newtype Api = Api {telegram :: ExceptT BotError (ReaderT (Logger IO, TelegramOpts) (StateT (Map.Map Integer Integer) IO)) ()}
data SettingsB =
  SettingsB
    { list_user :: Map.Map Integer Integer
    , config :: Config
    , logger :: Logger IO
    , log :: Stream
    , api :: ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
    }



data Settings = Settings
   { messenger  :: Maybe Text
   , pathConfig :: FilePath

   }

settingsP :: Parser Settings
settingsP =
  Settings <$> optional (argText "messendger"  "type messager")
           <*> argPath "config"  "path to config"


description :: Description
description = "bot"


