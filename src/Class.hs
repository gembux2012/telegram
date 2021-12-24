{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}

module Class where

import Control.Error
import Control.Error.Util (hoistEither)
import qualified Control.Monad as CM
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
-- (httpBS, setRequestMethod, setRequestQueryString, getResponseBody, httpLBS)

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, lift, liftIO, runReaderT)
import Data.Aeson (FromJSON, decodeStrict)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.Has (Has, getter)
import Data.Text.Internal.Lazy (Text)
import Error
import Network.HTTP.Client.Conduit (HttpException (..), HttpExceptionContent (..), Response (..), parseRequest, setRequestCheckStatus)
import Network.HTTP.Simple
import Network.HTTP.Types
--import Types
import qualified UnliftIO.Concurrent as U (threadDelay)
import Logger.Class (Log(..), Logger(..), logI)
import Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (runStateT, evalStateT, modify, get, StateT, StateT, StateT)
import Telegram.Types (Url(..), TelegramOpts(..), GetUpdates(..), Updates(..), SendMessage(..), Update(..),)
import App (responseToRequest)

  

class  Routable  q a | q -> a where
  toUrl ::  q -> Url
  toAPI ::
   (FromJSON a, Monad m, MonadIO m, MonadCatch m  ) =>
    q ->
    ExceptT BotError (ReaderT (Logger m, TelegramOpts) (StateT (Map.Map Integer Integer) m)) a
  toAPI q =
    catchE action checkError
    where
      action = do
        TelegramOpts host path _ <- asks snd
        req <- try $ lift $ responseToRequest host path  (toUrl q) 
        req' <- hoistEither $ first HTTPError req
        hoistEither $ note (ParserError $ show (getResponseBody req')) (decodeStrict (getResponseBody req'))

instance Routable  GetUpdates Updates where
  toUrl q =
    Url
      "GET "
       "getupdates"
      [ ("offset", bS $ offset q),
        ("limit", bS $ limit q),
        ("timeout", bS $ timeout q)
      ]

instance Routable  SendMessage Update where
  toUrl q =
    Url
      "GET"
      "sendmessage"
      [ ("chat_id", bS $ Just (chat_id q)),
        ("text", bS $ Just (text q)),
        ("reply_markup", bS $ Just (reply_markup q))
      ]

class Bytestrigable a where
  bS :: a -> Maybe BS8.ByteString

instance Bytestrigable (Maybe Integer) where
  bS (Just c) = Just (BS8.pack . show $ c)
  bS Nothing = Nothing

instance Bytestrigable (Maybe Int) where
  bS (Just c) = Just (BS8.pack . show $ c)
  bS Nothing = Nothing

instance Bytestrigable (Maybe String) where
  bS (Just c) = Just (BS8.pack $ c)
  bS Nothing = Nothing
  
instance Bytestrigable (Maybe ByteString) where
  bS (Just c) = Just  c
  bS Nothing = Nothing


