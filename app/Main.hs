{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Monad.State.Lazy (runStateT, evalStateT, modify, get)
import Text.Read (readMaybe)

--runBot :: ExceptT BotError (ReaderT Config IO) a -> IO ()
runBot api = do
  let list_user = Map.empty
  let config = Config "https://api.telegram.org/" 
                      "bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"
                      2
  res <- evalStateT (runReaderT (runExceptT  api ) config) list_user
  case res of
    Left (BotError err) -> putStrLn err
    Right _ -> putStrLn "bot stopped ok"
  return ()
  
        
main = runBot  telegram 

telegram  = do
  getUpdates (Just 20) Nothing (Just 20) Nothing 
  where 
     getUpdates  o l t a_u  = do
      liftIO $ putStrLn "awaiting message"
      (Updates update) <- toAPI $ GetUpdates o l t a_u
      unless (null update) do
        Config _ _ btn  <- ask
        user <- get
        mapM_
          ( \case
              Msg _ message
                -> do liftIO $ putStrLn $ "answer message: " <> mesText message
                      let id = fromId (mesFrom message)
                      let answer = prepareAnswer id (mesText message) user  btn
                      liftIO $ BS8.putStrLn $ snd answer
                      toAPI $ uncurry (SendMessage id) answer
              CallbackQ _ callback
               -> do liftIO $ putStrLn  "callback "
                     modify (Map.insert (fromId(cbFrom callback)) (read(cbData callback)))
                     toAPI $ SendMessage (fromId(cbFrom callback)) " good " "" 
          )
          update
        let last_id = update_id (last update) 
        getUpdates (Just (last_id + 1 :: Integer )) Nothing (Just 20) Nothing
      getUpdates (Just 20) Nothing (Just 20) Nothing  

--prepareAnswer ::  p -> [Char] -> Int -> ([Char], BS8.ByteString)
prepareAnswer from text user button = 
 case text of
  '/' : "repeat" -> ("how many times should I tell you ?", createButton)
  '/': "help" -> ("available commands: /help, /repeat","")
  '/': _   -> ("Unknown command, use /help ", "")    
  _ -> (concat[text <>", " | r <- [1..r]],"")
  where 
    createButton = LBS.toStrict $ encode $ Keyboard [ [1.. button+1] >>= \y -> [Key (show y) (show y)]]     
    r = fromMaybe 1 (Map.lookup from user) 
   

data Application m = Application 
 { logerr :: IO()
 
  }
  deriving stock (Generic)

{--
instance Has (Settings m) (Application m) where
  getter = config
  modifier f a = a {config = f . config $ a}
--}
