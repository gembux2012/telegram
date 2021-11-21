{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LBS




main :: IO ()

--runBot :: (Monad m, MonadIO IO) => ExceptT BotError (ReaderT (Application m) m) a -> IO ()
--runBot:: (Show a, Show b) =>                 ExceptT                   BotError (ReaderT (Application (a1 -> IO ()), Config) IO) a2                 -> IO ()
runBot api = do
  let config = Config "https://api.telegram.org/" 
                      "bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"
                      2
  res <- runReaderT (runExceptT  api ) config
  case res of
    Left (BotError err) -> putStrLn err
    Right _ -> putStrLn "bot stopped ok"
  return ()
  
        
        
main = runBot  telegram 

--telegram :: (MonadIO m) => ExceptT BotError m b
telegram  = do
  --c <- ask 
  getUpdates (Just 20) Nothing (Just 20) Nothing 
  where 
     getUpdates  o l t a_u  = do
      config <- ask
      liftIO $ putStrLn "waiting for an answer"
      --liftIO $ putStrLn c
      (Updates update) <- toAPI $ GetUpdates o l t a_u
      unless (null update) do
        mapM_
          ( \Update' {..} ->
              do
                liftIO $ putStrLn $ "answer message: " <> mesText
                let id = fromId (mesFrom message)
                let answer = prepareAnswer id mesText  mesData   
                toAPI $ SendMessage id mesText  ""
          )
          update
      let last_id = update_id (last update) 
      getUpdates (Just (last_id + 1 :: Integer )) Nothing (Just 20) Nothing
        

--prepareAnswer :: (Num (Maybe a -> a), Enum (Maybe a -> a), Show (Maybe a -> a)) => p -> [Char] -> a -> ([Char], ByteString)
prepareAnswer from text button = do
 case text of
  ['\\', 'r', 'e', 'p', 'e', 'a', 't'] ->
    ("how many times will you repeat ?", createButton) 
  where 
    createButton = LBS.toStrict $ encode $  [1..(fromMaybe button)+1] >>= \y -> [[Key (show y) (show y)]]
       
 
   

data Application m = Application 
 { logerr :: IO()
 
  }
  deriving stock (Generic)

{--
instance Has (Settings m) (Application m) where
  getter = config
  modifier f a = a {config = f . config $ a}
--}
