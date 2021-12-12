{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances  #-}

module Config where

import           Control.Error         (ExceptT, catchE, hoistEither, note)
import           Control.Monad.Catch
import           Control.Monad.Reader  (lift)
import           Data.Aeson
import           Data.Bifunctor        (first)
import qualified Data.ByteString.Char8 as BS
import           Error
import           Logger.Types

warning :: [Char]
warning = ", default values will be used!!."

--readConfig :: Monad m => [Char] -> ExceptT BotError IO Config
readConfig path =
  catchE action checkError
  where
    action = do
      content <- lift $ try $ BS.readFile path
      content' <- hoistEither $ first ConfigError content
      return $ hoistEither $ note (ConfigErrorJson  "error in config file!!" ) (decodeStrict content' :: Maybe Config)
