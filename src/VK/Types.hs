{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module VK.Types
 
where

import GHC.Generics
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson
import Data.Foldable


import Data.Aeson.Types (FromJSON)
import qualified Data.Text as T
import Network.HTTP.Simple (Query)
import Control.Monad.Cont (when)

import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import Text.Read (readMaybe)




data VKOpts = VKOpts
 { idGroup :: String ,
   keyGroup :: String,
   keyUser :: String,
   versionService :: String,
   uriVK :: String
 }
 deriving (Generic,  FromJSON, Show) 
 
data Action = Action
  { type' :: String,
    payload :: String,
    label :: String
  } 
 deriving (Show)

instance ToJSON Action where
  toJSON Action{..} = object [
    "type" .= type',
    "payload"  .= payload,
    "label" .= label 
    ]  


data GetAccess = GetAccess
 { access_token :: String,
   group_id :: String,
   v :: String
 }

data Message  = Message 
 { ts' :: String,
   updates :: [MessageUpdates]
   
 } | Response{ response :: Integer  }
   | Failed {failed :: Failed'}
   | ErrorVK {error :: ErrVK}          
   | NoMessage
   
  deriving ( Show) 

data ErrVK= ErrVK 
 { error_code :: Int,
   error_msg :: String 
 } 
 deriving (Generic, FromJSON,  Show)
 
data Access = Access     
  {   
   key :: String,
   server :: String,
   ts :: String
   }
 deriving (Generic, FromJSON, Show) 

data Failed' = Failed'
 {failed' :: Int,
  ts'' :: Maybe Int
  }  
 deriving ( Show)
 

instance FromJSON Message   where
  parseJSON  = withObject "message or response or access or filed" $ \o  ->  asum [
        Message <$> o .: "ts" <*> o .: "updates" , 
        Response <$> o .: "response"  ,
        Failed  <$> o .: "failed"  
         ]
         
                    

instance FromJSON Failed' where
  parseJSON = withObject " failed " $ \o -> do
    failed' <- o .: "failed"
    ts'' <-   o .:? "ts"
    return Failed' {..}
              


data MessageUpdates = MessageUpdates
  { _type :: String,
    from_id :: Integer,
    text :: String,
    _payload :: String
  }
  deriving (Generic, Show)
  
instance FromJSON MessageUpdates where
   parseJSON = withObject " message " $ \o -> do
     _type <- o .: "type" 
     obj <- o .: "object" 
     case _type of
      "message_new" -> do
        message <- obj .: "message"  
        from_id <- message .: "from_id"
        text <- message .: "text"
        _payload <- do 
         pl <-  message .:? "payload"
         case  pl :: Maybe String of
           Just _ -> return text     
           Nothing -> return ""
           
        --let _payload = ""
        return MessageUpdates{..}
      _ -> do 
        let from_id = 0
        let text = ""
        let _payload = ""
        return MessageUpdates{..}  
  

     
     
{--
data MessageObject = MessageObject
 { id :: Int,
   from_id :: Maybe Integer,
   owner_id :: Maybe Integer,
   date :: Maybe Integer,
   marked_as_ads :: Maybe Integer,
   post_type :: Maybe String,
   text :: Maybe String,
   can_edit :: Maybe Int,
   created_by :: Maybe Integer,
   can_delete :: Maybe Int,
   comments :: Maybe MessageComment,
   out  :: Maybe Int,
   user_id  :: Maybe Integer,
   read_state  :: Maybe Int,
   title  :: Maybe String,
   body  :: Maybe String 
   -- random_id  :: Int
  --owner_ids  :: []
  }
  deriving (Generic, FromJSON, Show)
--}
data MessageComment = MessageComment
  { count :: Int
  }
  deriving (Generic, FromJSON, Show)


