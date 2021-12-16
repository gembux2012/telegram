{-# LANGUAGE OverloadedStrings #-}

module Types where

import Turtle
import Prelude hiding (FilePath)


data Settings = Settings
   { messenger  :: Text
   , pathConfig   :: FilePath
   
   }

settingsP :: Parser Settings
settingsP = 
  Settings <$> optText "messendger" 'm' "type messager"
           <*> optPath "config" 'c' "path to config"


description :: Description
description = "bot"


