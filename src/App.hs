
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module App where

import qualified Control.Monad as CM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
-- (Response(..), Url(..), Error(..), getUpdates, Queryable(..) )

import Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString.Char8 as BS8
--import Network.HTTP.Client.Conduit (HttpException, parseRequest, httpLbs, HttpExceptionContent(..), )
--import Network.HTTP.Conduit
--import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpBS, setRequestMethod, setRequestQueryString, HttpException(..))
import Types
import Data.ByteString.Char8 (ByteString)
import Control.Error
import Control.Error.Util(hoistEither)
import Data.Bifunctor (first)
import Network.HTTP.Client.Conduit (HttpException(..), parseRequest,  setRequestCheckStatus, HttpExceptionContent(..), Response(..))
import qualified UnliftIO.Concurrent as U (threadDelay)
import Error
import Network.HTTP.Simple -- (httpBS, setRequestMethod, setRequestQueryString, getResponseBody, httpLBS)
import Network.HTTP.Types
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO, MonadIO, lift, asks)
import Data.Char (toLower)
import Data.Text.Internal.Lazy (Text)

responseToRequest :: (Monad m , MonadIO m , MonadThrow m , MonadCatch m) 
                      => Url -> m (Response BS8.ByteString)
responseToRequest Url {..} =do
     request' <- parseRequest requestPath
     --liftIO $ print request'
     httpBS $ setRequestMethod requestMethod $
              setRequestQueryString  requestQS $ 
              setRequestCheckStatus request' 
              -- setRequestResponseTimeout  (20 :: Integer) request' $


class Routable  q a | q -> a where
  toUrl ::  q -> Config -> Url
  toAPI :: (FromJSON a, Monad m , MonadIO m, MonadCatch m, Setting m)
            =>  q -> ExceptT BotError m a
  toAPI  q =
    catchE  action  checkError
     where 
      action = do
       set <- lift getSetting
       req <- try $ liftIO $ responseToRequest $ toUrl q set
       req' <- hoistEither $ first HTTPError req
       hoistEither $ note (ParserError $ show req') (decodeStrict (getResponseBody req'))

        
instance Routable  GetUpdates Response' where
         toUrl q s  = Url "GET " 
                  (url s <> "getupdates") 
                   [("offset" , bS $ offset q) ,
                    ("limit", bS $ limit q),
                    ("timeout", bS $ timeout q)  
                    ] 
instance Routable SendMessage Response' where
          toUrl q s  = Url  "GET" 
                      (url s <> "sendmessage")  
                      [("chat_id", bS $ Just (chat_id q)),
                       ("text", bS  $ Just (text q))
                       ]                                            




class Bytestrigable a where
 bS :: a -> Maybe BS8.ByteString

instance Bytestrigable  (Maybe Integer) where
         bS  (Just c) =  Just (BS8.pack . show $ c)
         bS Nothing  = Nothing
instance Bytestrigable (Maybe Int) where
         bS  (Just c) =  Just (BS8.pack . show $ c)
         bS Nothing  = Nothing
instance Bytestrigable (Maybe String) where 
         bS  (Just c) =  Just (BS8.pack $ c)
         bS Nothing  = Nothing
 
  


newtype Settings m = Settings {doGetConfig :: m Config}
  
class Monad m  => Setting m where
  getSetting ::  m Config

instance
   ( Has (Settings m) r,
    Monad m,
    MonadIO m
  ) =>
  Setting (ReaderT r m)
   where
   getSetting =  asks getter >>= \(Settings doGetConf  ) -> lift doGetConf