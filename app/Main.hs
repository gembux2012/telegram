{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE OverloadedStrings  #-}
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

newtype Logg m =  Logg  {pr ::String -> m()}

data Application  m = Application 
 { logger :: Logger  m
 }



  



pri c a  = putStrLn $ show  $ fromLoggable c <> a   
--runBot ::  ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) a1 -> IO ()
runBot api = do
  let c ="jhk"
  let logger = Logger{dologLn =  pri c }
  let list_user = Map.empty
  let config = Config "https://api.telegram.org/" 
                      "bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"
                      5
  res <- evalStateT (runReaderT (runExceptT  api ) (logger, config)) list_user
  case res of
    Left (BotError err) -> dologLn  logger (fromLoggable ERROR <> fromLoggable err)
    Right _ -> dologLn  logger (fromLoggable INFO <> fromLoggable " ")
  return ()

 
        
main = runBot  telegram
 
--telegram ::  ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
telegram  = do
  logI "это логгер" 
  getUpdates (Just 20) Nothing (Just 20) Nothing 

getUpdates :: MonadReader (Logger m ,Config) m => Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe [String] -> ExceptT BotError m ()
getUpdates  o l t a_u  = do
      --liftIO $ putStrLn "awaiting message"

      (Updates update) <- toAPI $ GetUpdates o l t a_u
      unless (null update) do

        --let btn = 2
        user <- get
        mapM_
          ( \case
              Msg _ message
                -> do liftIO $ putStrLn $ "answer message: " <> mesText message
                      Config _ _ btn  <- asks snd
                      let id = fromId (mesFrom message)
                      let answer = prepareAnswer id (mesText message) user  btn
                      liftIO $ BS8.putStrLn $ snd answer
                      toAPI $ uncurry (SendMessage id) answer
              CallbackQ _ callback
               -> do liftIO $ putStrLn  "callback "
                     modify (Map.insert (fromId(cbFrom callback)) (read(cbData callback)))
                     toAPI $ SendMessage (fromId(cbFrom callback)) " good " (BS8.pack "") 
          )
          update
        let last_id = update_id (last update) 
        getUpdates (Just (last_id + 1 :: Integer )) Nothing (Just 20) Nothing
      getUpdates (Just 20) Nothing (Just 20) Nothing  

--prepareAnswer ::  p -> [Char] -> Int -> ([Char], BS8.ByteString)
prepareAnswer from text user button = 
 case text of
  '/' : "repeat" -> ("how many times should I tell you ?", createButton)
  '/': "help" -> ("available commands: /help, /repeat",BS8.pack "")
  '/': _   -> ("Unknown command, use /help ", BS8.pack "")    
  _ -> (concat[text <>" " | r <- [1..r]],BS8.pack "")
  where 
    createButton = LBS.toStrict $ encode $ Keyboard [ [1.. button] >>= \y -> [Key (show y) (show y)]]     
    r = fromMaybe 1 (Map.lookup from user) 
   


--newtype Application  m = Application
 -- { logger   :: Logger m
-- }

--instance Has  (Logger (StateT s m)) (Application s m, Config) where
 --getter   =  logger   
  --modifier f a = a {logger  = f . logger $ a}

