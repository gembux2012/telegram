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
import Network.HTTP.Client.Conduit (HttpException, parseRequest)
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpBS, setRequestMethod, setRequestQueryString)
import Types

runRoute ::
  MonadIO m =>
  --MonadThrow m =>
  MonadCatch m =>
  --Opts ->
  Url ->
  m (Either ErrorBot BS8.ByteString)
runRoute Url {..} = do
  request' <- parseRequest requestPath
  let request =
        setRequestMethod requestMethod $
          setRequestQueryString
            requestQS
            request'
  response <- try $ httpBS request
  case response of
    Left e -> pure $ Left (ErrorBot 2 (displayException (e :: HttpException)))
    Right r -> pure $ Right $ getResponseBody r


-- https://api.telegram.org/bot3012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/getupdates
class Routable q a | q -> a where
  toUrl :: q -> Url
  toAPI :: FromJSON a => MonadCatch m => MonadIO m => q -> m (Either ErrorBot a)
  toAPI q = do
    req <- runRoute $ toUrl q
    case req of
      Right r -> do
        case decodeStrict r of
          Just mess -> do
            pure $ Right mess
          Nothing -> do
            pure $ Left $ ErrorBot 0 ("can't decode: " <> show r)
      Left e -> pure $ Left e
-- _ -> do
--  pure $ Response $ Left $ Error 0 "it can't be"

instance Routable GetUpdates Response where
  toUrl q = Url  "Get" "https://api.telegram.org/bot3012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/" [("offset",  offset q),
                                                                                             ("limit", limit q)]
                   