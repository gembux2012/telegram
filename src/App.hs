{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module App where

import qualified Control.Monad as CM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class


import Types -- (Response(..), Url(..), Error(..), getUpdates, Queryable(..) )
import Network.HTTP.Client.Conduit (HttpException, parseRequest)
import Network.HTTP.Simple (httpBS, getResponseStatusCode, getResponseBody, setRequestMethod, setRequestQueryString)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS8

runRoute ::
  MonadIO m =>
  --MonadThrow m =>
  MonadCatch m =>
  --Opts ->
  Url ->
  m Response
runRoute  Url {..} = do
  request' <- parseRequest   requestPath
  let request =
        setRequestMethod requestMethod $
          setRequestQueryString
            requestQS
            request'
  response <- try $ httpBS request
  case response of 
   Left e -> pure $ Response (Left (Error 2 (displayException (e :: HttpException ))))
   Right r -> do     
     let  AR actionResponse = decodeStrict $ getResponseBody  r 
     case getResponseStatusCode r of
      200 -> do
        case decodeStrict . getResponseBody $ r :: Maybe Response of
          Just mess -> do
            pure mess
          Nothing -> do
            pure $ Response $ Left $ Error 0 ("can't decode: " <> show r)
      _ -> do
       pure $ Response $ Left $ Error 0 "it can't be"



{--
parseRequest' :: BS8.ByteString -> r -> r
parseRequest' body r =
 case decodeStrict  body :: Maybe Response of
           Just mess -> do
             pure mess
           Nothing -> do
             pure $ Response $ Left $ Error 0 ("can't decode: " <> show r)
             --}
-- https://api.telegram.org/bot3012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/getupdates
class  Routable  q a | q -> a where
  toUrl :: q -> Url
  toAPI :: Url -> a
  toAPI = runRoute . toUrl

instance Routable GetUpdates Response where
 toUrl = getUpdates




