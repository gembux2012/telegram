{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Logger.Class

where

import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Has (Has, getter)
import Data.Text (Text, append, pack)
import GHC.Stack.Types (HasCallStack)
import Logger.Types (Priority (..), Config)
import Control.Monad.RWS.Lazy (RWST, MonadIO, tell, liftIO, ask)
import Data.String
import Control.Monad.State.Lazy (StateT)
import Control.Error (ExceptT)
import Error (BotError)
import qualified Data.Map as Map




newtype Logger m = Logger {dologLn :: HasCallStack => Text -> m ()}
  
class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logI :: HasCallStack => Loggable a => a -> m ()
  logW :: HasCallStack => Loggable a => a -> m ()
  logE :: HasCallStack => Loggable a => a -> m ()

instance
 (Has  (Logger m) r ,
    Monad m
   -- MonadIO m 
  ) =>
  Log (ExceptT e (ReaderT r (StateT s m ))) where
  logI  a = asks getter >>= \(Logger doLog) ->lift $ lift $ lift . doLog $ fromLoggable INFO <> " " <>  fromLoggable  a
 -- logW a =
   -- asks getter >>= \(Logger doLog) -> lift . doLog $ fromLoggable WARNING <> " " <>  fromLoggable a
  --logE a =
   -- asks getter >>= \(Logger doLog) -> lift . doLog $ fromLoggable ERROR <> " " <> fromLoggable a



instance Loggable Text where
  fromLoggable = id

instance Loggable Priority  where
 fromLoggable = pack.show
 
instance Loggable String  where
 fromLoggable = pack  