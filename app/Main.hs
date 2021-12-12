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
import Control.Monad ()
--import Control.Monad.IO.Class ()
import Control.Monad.Reader (ReaderT, runReaderT, unless, asks)
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Error
--import GHC.Generics ()
--import GHC.Show (ShowS)

--import Data.Traversable (for)
import Data.Aeson (encode)
import  qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Control.Monad.State ( evalStateT, modify, get, StateT, StateT, StateT)
import Text.Read ()
import Logger.Class
--import Logger.Types (Priority(..), Config, Config)
import Control.Monad.Catch.Pure ()
import qualified Data.Text as T
import Logger.Types hiding (Config)
import Streams (initStdOutSrteam, msgStream, stopStream)
import Types --(Updates(..), Update'( .. ),Config(..), GetUpdates(..), mesText, mesFrom, SendMessage(..))
import Turtle
import Prelude hiding (FilePath,error,log)




main = initBot  

--runBot :: ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) a1 -> IO ()
initBot :: IO ()
initBot  = do
  (Settings api config_path) <- options description settingsP
   
  
  let list_user = Map.empty
  let config = Config "https://api.telegram.org/"
                      "bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"
                      5
  log <- initStdOutSrteam defaultConfig
  
  let logger = Logger $ msgStream log                  
  when (api == T.pack "telegram") do
      runBot telegram logger config list_user log
  msgStream log (fromLoggable INFO <> T.pack " unknown key --" <> api <> T.pack " will be launched bot for telegramm")
  runBot telegram logger config list_user log    
  stopStream log
  
runBot api logger config  list_user log =
 do 
     res <- evalStateT (runReaderT (runExceptT  api ) (logger, config)) list_user
     case res of
       Left (BotError error) ->  msgStream log (fromLoggable ERROR <> T.pack " " <> fromLoggable error)
       Right _ ->msgStream log (fromLoggable INFO <> T.pack " " <> fromLoggable " OK ")
   

telegram :: ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
telegram  = do
  logI "bot launched"
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
              Msg _ msg
                -> do logI $ "answer message: " <> mesText msg
                      let id = fromId (mesFrom msg)
                      let answer = prepareAnswer id (mesText msg) user  btn
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
   




