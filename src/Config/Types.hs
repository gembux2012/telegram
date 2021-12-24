{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Config.Types where

import GHC.Generics (Generic)

import Telegram.Types (TelegramOpts)
import Data.Aeson.Types (FromJSON)
import Logger.Types (LogOpts)

data Config  = Config 
  {logOpts :: LogOpts,
   telegramOpts :: Maybe TelegramOpts
  }
  deriving (Generic,  FromJSON, Show)

