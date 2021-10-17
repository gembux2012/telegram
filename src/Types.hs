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
   limit :: Maybe Integer,
   timeout :: Maybe Integer,
   allowed_updates :: Maybe [String]
 }

data Response = Response 
 { 
   ok :: String,
   result :: Maybe [Update],
   error_code :: Maybe Int,
   description  :: Maybe String 
 }
 deriving(Generic, FromJSON,  Show )
{--
instance FromJSON Response where
  parseJSON = withObject " response " $ \o -> do
    ok :: String <- o  .: "ok"
    result <-    
     if  ok == "true" 
     then  do
      r <- o .: "result"
      res <-  r .: "updates"  
      return $ Right res 
     else do 
      error_code <- o .: "error_code"
      description <-  o .: "description"
      return $ Left Error {..}    
    return Response{..} 
--}
     
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
