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

--GetUpdates :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe [String] -> GetUpdates 
data GetUpdates   = GetUpdates 
 { offset :: Maybe Integer,
   limit :: Maybe Integer,
   timeout :: Maybe Integer,
   allowed_updates :: Maybe [String]
 }

data SendMessage = SendMessage
 { chat_id :: Integer,
   text :: String
 } 
 deriving  Show
  
data Response' = Response' 
 { result ::  [Update]
 } | Error 
 { error_code ::  Int,
   description :: String
 }
  | NoResponse
 deriving  Show 

instance FromJSON Response' where
  parseJSON =  withObject "response or error" $ \o -> 
   asum [Response' <$> o .: "result",
         Error <$> o .: "error_code" <*> o .: "description " ]

     
data Update = Update
 { update_id :: Integer,
   message   :: Message
 }
 deriving (Generic,  FromJSON, Show)
 
data Message = Message 
  { mesMessageId :: Integer,
    mesFrom       :: From,
    --"chat "      :: Chat,
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
