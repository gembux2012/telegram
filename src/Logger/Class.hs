{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Logger.Class

where

import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Has (Has, getter)
import Data.Text (Text, append, pack)
import GHC.Stack.Types (HasCallStack)
import Control.Monad.RWS.Lazy (RWST, MonadIO, tell, liftIO, ask)
import Data.String
import Control.Monad.State.Lazy (StateT)
import Control.Error (ExceptT)
--import Error (BotError)
import qualified Data.Map as Map
import Logger.Types (Priority(..))




newtype Logger m = Logger {dologLn :: HasCallStack => Priority -> Text -> m ()}
  
class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logX :: HasCallStack => Loggable a => Priority -> a  -> m ()
  logI :: HasCallStack => Loggable a => a -> m ()
  logW :: HasCallStack => Loggable a => a -> m ()
  logE :: HasCallStack => Loggable a => a -> m ()

instance
 (Has  (Logger m) r ,
    Monad m
  ) =>
  Log (ExceptT e (ReaderT r (StateT s m ))) where
  logX pr a   = asks getter >>= \(Logger doLog) ->  lift $ lift $ lift . doLog  pr $ fromLoggable pr <> " " <>  fromLoggable  a
  logI = logX INFO 
  logW = logX WARNING
  logE = logX ERROR

--instance Log IO where
 --logX pr a  log = log pr $ fromLoggable pr <> " " <>  fromLoggable  a 
  

instance Loggable Text where
  fromLoggable = id

instance Loggable Priority  where
 fromLoggable = pack.show
 
instance Loggable String  where
 fromLoggable = pack  