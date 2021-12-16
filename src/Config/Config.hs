{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances  #-}

module Config.Config where

import           Control.Error             (ExceptT, catchE, hoistEither, note,
                                            runExceptT)
import           Control.Monad.Catch
import           Control.Monad.Reader      (lift)
import           Data.Aeson
import           Data.Bifunctor            (first)
import qualified Data.ByteString.Char8     as BS
import           Data.Text                 (unpack)
import           Error
import           Filesystem.Path.CurrentOS (toText)
import           Logger.Types
import           Prelude                   hiding (FilePath)
import           Turtle                    (FilePath, encodeString)
import Config.Types

--readConfig ::  Monad m => String -> ExceptT BotError  m Config
readConfig :: FilePath -> IO (Either String Config)
readConfig path =
  case toText path of
   Left e -> return $ Left "invalid path to config faile"
   Right p -> do
     content <- try (BS.readFile (unpack p)) :: IO (Either IOError BS.ByteString)
     case content of
        Left e -> return $ Left (show e)
        Right content' ->
          case decodeStrict content' :: Maybe Config of
            Just config -> return $ Right config
            Nothing     -> return $ Left "Invalid config file!"
