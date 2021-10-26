
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Network.HTTP.Client.Conduit (HttpException(..), parseRequest,  setRequestCheckStatus, HttpExceptionContent(..), Response(..), )
import qualified UnliftIO.Concurrent as U (threadDelay)
import Error
import Network.HTTP.Simple --(httpBS, setRequestMethod, setRequestQueryString, getResponseBody, httpLBS)
import Network.HTTP.Types.Status
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO, MonadIO, lift, asks)

responseToRequest ::
 (Monad m , MonadIO m , MonadThrow m , MonadCatch m) =>
  Url -> m (Response BS8.ByteString)
responseToRequest Url {..} =do
     request' <- parseRequest requestPath
     httpBS $ setRequestMethod requestMethod $
              setRequestQueryString  requestQS $ 
              setRequestCheckStatus request'


class Routable  q a | q -> a where
  toUrl :: q -> Config -> Url
  toAPI :: (FromJSON a, Monad m , MonadIO m, MonadCatch m, Setting m) =>  q -> ExceptT BotError m a
  toAPI  q =
    catchE  action  checkError
     where 
      action = do
       set <- lift getSetting
       req <- try $ liftIO $ responseToRequest $ toUrl q set 
       req' <- hoistEither $ first HTTPError req
       hoistEither $ note (ParserError $ show req') (decodeStrict (getResponseBody req'))
       
     
          
--    liftIO $ putStrLn $ "ConnectionFailure, next connection attempt in 30 seconds" <> show e
    --    U.threadDelay (10^6 * 30)
       --  hoistEither $ Left $ BotError "connection failure"         
        
instance Routable GetUpdates Response' where
  
  toUrl q s  = Url "GET "  (url s)  [("offset",  offset  q),
                                            ("limit", limit   q)] 


designerUrl q = ("https://api.telegram.org/bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/getupdates",
 [("offset",  offset  q),
 ("limit", limit   q)])


newtype Set m = Set {doGetConfig :: m Config}
  
class Monad m  => Setting m where
  getSetting ::  m Config
  
instance
   ( Has (Set m) r,
    Monad m,
    MonadIO m
  ) =>
  Setting (ReaderT r m)
   where
   getSetting =  asks getter >>= \(Set doGetConf  ) -> lift doGetConf   