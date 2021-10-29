{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.Types (FromJSON, parseJSON, (.:), withObject, genericParseJSON)
import Network.HTTP.Simple (Query)
import qualified Data.ByteString.Char8 as BS8
import Control.Monad (when, void, void)
import Data.Void (Void)
import Data.Aeson.Casing
import Data.Foldable

newtype Config = Config 
  { url :: String
  }

data Url= Url
 { --requestHost :: BS8.ByteString,
   requestMethod :: BS8.ByteString,
   requestPath :: String,--BS8.ByteString,
   requestQS :: Query 
 }
 
data GetUpdates   = GetUpdates 
 { offset :: Maybe Integer,
   limit :: Maybe Integer,
   timeout :: Maybe Integer,
   allowed_updates :: Maybe [String]
 }

data SendMessage = SendMessage
 { chat_id :: Integer,
   text :: String,
   reply_markup :: String
 } 
 deriving  Show
 -- {
     --            "inline_keyboard": [
     --                [
     --                    {"text": "Yes", "callback_data": "1"},
     --                    {"text": "No", "callback_data": "2"}
     --                ]
     --            ]
     --        }
  
newtype Updates
  = Updates {result :: [Update']}
  deriving (Generic , FromJSON, Show) 

newtype Update
  = Update {updateResult :: Message}
  deriving (Generic ,  Show)

instance FromJSON Update where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase   
   
     
data Update' = Update'
 { update_id :: Integer,
   message   :: Message
 }
 deriving (Generic,  FromJSON, Show)
 
data Message = Message 
  { mesMessageId :: Integer,
    mesFrom       :: From,
    mesChat       :: Chat,
    mesDate       :: Integer,
    mesText       :: String  
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


