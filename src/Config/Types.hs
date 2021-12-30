{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Config.Types where

import GHC.Generics (Generic)

import Telegram.Types (TelegramOpts)
import Data.Aeson.Types (FromJSON)
import Logger.Types (LogOpts)
import VK.Types (VKOpts)

data Config  = Config 
  {logOpts :: LogOpts,
   telegramOpts :: Maybe TelegramOpts,
   vkOpts :: Maybe VKOpts
  }
  deriving (Generic,  FromJSON, Show)

