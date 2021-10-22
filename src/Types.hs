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



data ActionResponse r = AR { actionResponse :: r}

data Url= Url
 { --requestHost :: BS8.ByteString,
   requestMethod :: !BS8.ByteString,
   requestPath :: !String,--BS8.ByteString,
   requestQS :: !Query 
 }
 
-- https://api.telegram.org/bot3012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/getupdates          

data GetUpdates   = GetUpdates 
 { offset :: Maybe BS8.ByteString,
   limit :: Maybe BS8.ByteString,
   timeout :: Maybe BS8.ByteString,
   allowed_updates :: Maybe [String]
 }
 deriving  Show 
data Response = Response 
 { result ::  [Update]
 } | Error 
 { error_code ::  Int,
   description :: String
 }
  | NoResponse
 deriving  Show 

instance FromJSON Response where
  parseJSON =  withObject "response or error" $ \o -> 
   asum [Response <$> o .: "result",
         Error <$> o .: "error_code" <*> o .: "description " ]

     
data Update = Update
 { update_id :: Integer,
   message   :: Message
 }
 deriving (Generic,  FromJSON, Show)
 
data Message = Message 
  { message_id :: Integer,
    from       :: From,
    --"chat "      :: Chat,
    date       :: Integer,
    text       :: String  
  }
  deriving (Generic,  FromJSON, Show)
  
data From = From 
  { fromId :: Integer,
    fromIsBot :: Bool,
    fromFirstName :: Text
  }
  deriving (Generic,Show)
  
instance FromJSON From where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase
    

{--  
data Error = Error
 { error_code :: Int,
   description  :: String
 }  
 deriving (Generic,   Show) 
--} 
data ErrorBot = ErrorBot 
 { id_error :: Int,
   err_msg :: String
 } 
 deriving Show 
 
 {--         "chat": {
           "id": 990022354,
           "first_name": "Алексей",
           "type": "private"
         },
 --}
