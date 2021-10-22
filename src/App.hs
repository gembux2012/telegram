{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App where

import qualified Control.Monad as CM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
-- (Response(..), Url(..), Error(..), getUpdates, Queryable(..) )

import Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString.Char8 as BS8
--import Network.HTTP.Client.Conduit (HttpException, parseRequest, httpLbs, HttpExceptionContent(..), )
--import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpBS, setRequestMethod, setRequestQueryString, HttpException)
import Types
import Data.ByteString.Char8 (ByteString)
import Control.Error
import Control.Error.Util(hoistEither)
import Data.Bifunctor (first)
import Network.HTTP.Client.Conduit (HttpException(..), parseRequest, HttpExceptionContent, HttpExceptionContent(..), parseRequest_, setRequestCheckStatus)
import qualified UnliftIO.Concurrent as U (threadDelay)
import Error

{--
runRoute ::
  MonadIO m =>
  --MonadThrow m =>
  MonadCatch m =>
  --Opts ->
  Url ->
  m (Either String BS8.ByteString)

 -}





responseToRequest ::
 Monad m => MonadIO m => MonadThrow m => MonadCatch m =>
 Url ->
  m (Either HttpException  BS8.ByteString)
responseToRequest Url {..} =do
      request' <- parseRequest requestPath
      
   --  let request =
     --       setRequestMethod requestMethod $
       --     setRequestQueryString  requestQS  request'
      --liftIO $ putStrLn $ show  request
      
      response <-  try $ httpBS $ setRequestMethod requestMethod $
                                  setRequestQueryString  requestQS $ setRequestCheckStatus request'
      case response of
        Left e -> pure  $ Left  e
        Right r -> pure $ Right $ getResponseBody r
--}

-- https://api.telegram.org/bot3012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/getupdates
class Routable q a | q -> a where
  toUrl :: q -> Url
  toAPI :: FromJSON a => Monad m => MonadIO m => q -> ExceptT APIError m a
  toAPI q =
    catchE  action  checkError
     where 
      action = do
       req <- liftIO $ responseToRequest $ toUrl q
       req' <- hoistEither $ first HTTPError req
       hoistEither $ note (ParserError $ show req') (decodeStrict req')
    --  checkError  (HTTPError(HttpExceptionRequest _  (ConnectionFailure e))) = do
    --    liftIO $ putStrLn $ "ConnectionFailure, next connection attempt in 30 seconds" <> show e
    --    U.threadDelay (10^6 * 30)
    --    toAPI q
      ccheckError  (HTTPError(HttpExceptionRequest _  (StatusCodeException _  cod))) = do
       hoistEither $ Left $ OtherHttpError $ show cod    
      checkError  (HTTPError(HttpExceptionRequest _ content)) = do
       hoistEither $ Left $ OtherHttpError $ show content    
      checkError  (HTTPError(InvalidUrlException url err)) = do
             hoistEither $ Left $ OtherHttpError $ show $  err     
      
       
      checkError (ParserError err) = do 
        liftIO $ putStrLn  "can not parse" 
        hoistEither $ Left $ ParserError err 
        
instance Routable GetUpdates Response where
  toUrl q = Url  "GET" "https://api.telegram.org/ot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/getupdates" [("offset",  offset  q),
                                                                                             ("limit", limit   q)]
-- checkError :: Monad m => MonadIO m => q -> APIError -> ExceptT e m a

