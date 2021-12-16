{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module App where


import           Control.Error
import           Control.Error.Util          (hoistEither)
import           Control.Monad
import qualified Control.Monad               as CM
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT,
                                              ask, asks, lift, liftIO,
                                              runReaderT)
import           Control.Monad.State.Lazy    (StateT, evalStateT, get, modify,
                                              runStateT)
import           Data.Aeson                  (encode)
import           Data.Bifunctor              (first)
import           Data.ByteString.Char8       (pack)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import           Data.Char                   (toLower)
import           Data.Has                    (Has, getter)
import qualified Data.Map                    as Map
import           Data.Text.Internal.Lazy     (Text)
import           Error
import           Logger.Class                (Log (..), Logger (..), logI)
import           Network.HTTP.Client.Conduit (HttpException (..),
                                              HttpExceptionContent (..),
                                              Response (..), parseRequest,
                                              setRequestCheckStatus)
import           Network.HTTP.Simple
import           Network.HTTP.Types
import           Telegram.Types              (Key (..), Keyboard (..), Url (..))
import qualified UnliftIO.Concurrent         as U (threadDelay)



responseToRequest ::
 (Monad m, MonadIO m, MonadThrow m, MonadCatch m ) =>
  String -> String -> Url ->
 m (Response BS8.ByteString)
responseToRequest  host  path Url {..}  = do
  request' <- parseRequest  (host <> path <> requestPath)
  let request = setRequestMethod requestMethod
                $ setRequestQueryString requestQS
                $ setRequestCheckStatus request'
  httpBS request


prepareAnswer from text user button =
 case text of
  '/' : "repeat" -> ("how many times should I tell you ?", createButton)
  '/': "help"    -> ("available commands: /help, /repeat",BS8.pack "")
  '/': _         -> ("Unknown command, use /help ", BS8.pack "")
  _              -> (concat[text <>" " | r <- [1..r]],BS8.pack "")
  where
    createButton = LBS.toStrict $ encode $ Keyboard [ [1.. button] >>= \y -> [Key (show y) (show y)]]
    r = fromMaybe 1 (Map.lookup from user)


