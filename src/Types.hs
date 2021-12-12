{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson.Types (FromJSON, parseJSON, (.:), genericParseJSON, ToJSON, toJSON, fieldLabelModifier, genericToJSON, withObject)
import Network.HTTP.Simple (Query)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad (when, void, void)
import Data.Void (Void)
import Data.Aeson.Casing
import Data.Foldable
import Data.ByteString.Char8 (ByteString)
import Turtle
import Prelude hiding (FilePath)

data Config = Config
  { host :: String,
    path :: String,
    button :: Int
  
  }

data Url= Url
 { --requestHost :: BS8.ByteString,
   requestMethod :: BS8.ByteString,
   requestPath :: String,
   requestQS :: Query 
 }
 
data GetUpdates   = GetUpdates 
 { 
   offset :: Maybe Integer,
   limit :: Maybe Integer,
   timeout :: Maybe Integer,
   allowed_updates :: Maybe [String]
 }

data SendMessage = SendMessage
 { chat_id ::  Integer,
   text ::  String,
   reply_markup :: BS8.ByteString
 } 
 deriving  Show
 
newtype Keyboard = Keyboard {inline_keyboard :: [[Key]] }
 deriving (Generic , ToJSON,  Show)

data Key = Key
 { keyText :: String,
   keyCallbackData :: String
 }
 deriving (Generic ,  Show)

instance ToJSON Key where
  toJSON = genericToJSON $ aesonPrefix snakeCase
     
instance FromJSON Key where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase       

newtype  Updates  = Updates {result :: [Update'] }
 deriving (Generic, FromJSON,  Show)




newtype Update
  = Update {updateResult :: Message}
  deriving (Generic ,  Show)

instance FromJSON Update where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase   
   
     
data Update' = Msg
 { update_id :: Integer,
   message   ::  Message
 } | CallbackQ
 { update_id :: Integer,
   callback_query   :: Callback
 }
 deriving  Show

instance FromJSON Update' where
  parseJSON = withObject "msg or callback" $ \o -> asum [
    Msg <$> o .: "update_id" <*> o .: "message",
    CallbackQ <$> o .: "update_id" <*> o .: "callback_query" ]

data  Callback = Callback
   {cbFrom :: From,
    cbData  :: String
   }
 deriving (Generic,  Show)

instance FromJSON Callback where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
 
data Message = Message 
  { mesMessageId :: Integer,
    mesFrom       :: From,
     mesChat   :: Chat,
     mesDate   :: Integer,
     mesText   :: String,
     mesData   :: Maybe Integer
  }
  deriving (Generic,   Show)
  
instance FromJSON Message where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase  
  
data From = From 
  { fromId :: Integer,
    fromIsBot :: Bool,
    fromFirstName :: Text
  }
  deriving (Generic,Show)
 
instance FromJSON From where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
    

data ErrorBot = ErrorBot 
 { id_error :: Int,
   err_msg :: String
 } 
 deriving Show 
 
data Chat = Chat
 { chatId :: Integer,
   chatFirstName :: String,
   chatType :: String
  }
  deriving (Generic,Show)
 
instance FromJSON Chat where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase


data Settings = Settings
   { messenger  :: Text
   , pathConfig   :: FilePath
   
   }

settingsP :: Parser Settings
settingsP = 
  Settings <$> argText "messendger"  "type messager"
           <*> argPath "cfg" "path to config"


description :: Description
description = "bot"
