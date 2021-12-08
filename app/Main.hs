{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import App
--(Response'(..), Update(..), Config(..), GetUpdates(..), SendMessage(..), GetUpdates, mesText, From(..))

import Control.Error (ExceptT, runExceptT)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, ReaderT, ask, lift, liftIO, runReaderT, unless, MonadReader, Reader, asks)
import Data.Has (Has, getter, modifier)
import Data.Maybe (fromMaybe)
import Error
import GHC.Generics (Generic)
import GHC.Show (ShowS)
import Types
import Data.Traversable (for)
import Data.Aeson (encode)
import  qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Control.Monad.State.Lazy (runStateT, evalStateT, modify, get, StateT, StateT, StateT)
import Text.Read (readMaybe)
import Logger.Class
import Logger.Types (Priority(..))
import Control.Monad.Catch.Pure (MonadCatch)
import qualified Data.Text as T
import Streams
import Logger.Types hiding (Config)


main = runBot  telegram

runBot :: ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) a1 -> IO ()
runBot api = do
  let list_user = Map.empty
  let config = Config "https://api.telegram.org/" 
                      "bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"
                      5
  l <- initLogger defaultConfig 
  let logger = Logger $ logMessage l                  
  res <- evalStateT (runReaderT (runExceptT  api ) (logger, config)) list_user
  case res of
    Left (BotError err) ->  logMessage l (fromLoggable ERROR <> T.pack " " <> fromLoggable err)
    Right _ ->logMessage l (fromLoggable INFO <> T.pack " " <> fromLoggable " OK ")
  logStop l

telegram :: ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
telegram  = do
  Config _ _ btn  <- asks getter
  getUpdates (Just 20) Nothing (Just 20) Nothing 

getUpdates :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe [String] -> ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
getUpdates  o l t a_u  = do
      Config _ _ btn  <- asks getter
      logI "awaiting message"
      (Updates update) <- toAPI $ GetUpdates o l t a_u
      unless (null update) do
        user <- get
        mapM_
          ( \case
              Msg _ message
                -> do logI $ "answer message: " <> mesText message
                      let btn = 2
                      let id = fromId (mesFrom message)
                      let answer = prepareAnswer id (mesText message) user  btn
                      logI  "send message"
                      toAPI $ uncurry (SendMessage id) answer
              CallbackQ _ callback
               -> do logI "callback "
                     modify (Map.insert (fromId(cbFrom callback)) (read(cbData callback)))
                     logI  "send message"
                     toAPI $ SendMessage (fromId(cbFrom callback)) " good " (BS8.pack "") 
          )
          update
        let last_id = update_id (last update) 
        getUpdates (Just (last_id + 1 :: Integer )) Nothing (Just 20) Nothing
      getUpdates (Just 20) Nothing (Just 20) Nothing  

prepareAnswer from text user button = 
 case text of
  '/' : "repeat" -> ("how many times should I tell you ?", createButton)
  '/': "help" -> ("available commands: /help, /repeat",BS8.pack "")
  '/': _   -> ("Unknown command, use /help ", BS8.pack "")    
  _ -> (concat[text <>" " | r <- [1..r]],BS8.pack "")
  where 
    createButton = LBS.toStrict $ encode $ Keyboard [ [1.. button] >>= \y -> [Key (show y) (show y)]]     
    r = fromMaybe 1 (Map.lookup from user) 
   




