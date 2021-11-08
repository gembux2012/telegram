{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import App
--(Response'(..), Update(..), Config(..), GetUpdates(..), SendMessage(..), GetUpdates, mesText, From(..))

import Control.Error (ExceptT, runExceptT)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, ReaderT, ask, lift, liftIO, runReaderT, unless, MonadReader, Reader)
import Data.Has (Has, getter, modifier)
import Data.Maybe (fromMaybe)
import Error
import GHC.Generics (Generic)
import GHC.Show (ShowS)
import Types
import Data.Traversable (for)
import Data.Aeson.Encode (encode)
import Data.ByteString.Char8 (ByteString)




main :: IO ()

--runBot :: (Monad m, MonadIO IO) => ExceptT BotError (ReaderT (Application m) m) a -> IO ()
runBot:: Show a =>                 ExceptT                   BotError (ReaderT (Application (a1 -> IO ()), Config) IO) a2                 -> IO ()
runBot api = do
  let config = Config "https://api.telegram.org/" 
                      "bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"
                      2
  res <- runReaderT (runExceptT $ api ) (app, config)
  case res of
    Left (BotError err) -> putStrLn err
    Right _ -> putStrLn "bot stopped ok"
  return ()
  where
    --app :: Config -> Application IO
    app  =
      Application
        {  logerr  =  print 
        }
        
main = runBot  telegram 

telegram :: (MonadIO m) => ExceptT BotError m b
telegram  =
  getUpdates (Just 20) Nothing (Just 20) Nothing
  where 
     getUpdates  o l t a_u = do
      liftIO $ putStrLn "waiting for an answer"
      (Updates update) <- toAPI $ GetUpdates o l t a_u
      unless (null update) do
        mapM_
          ( \Update' {..} ->
              do
                liftIO $ putStrLn $ "answer message: " <> mesText message 
                toAPI $ SendMessage (fromId (mesFrom message)) (mesText message) ""
          )
          update
      let last_id = update_id (last update) 
      getUpdates (Just (last_id + 1 :: Integer )) Nothing (Just 20) Nothing
        
--createButton :: Int -> ByteString 
--createButton  y = encode $  [1..y+1] >>= \y -> [[Key (show y) (show y)]]
    
  

data Application m = Application 
 { logerr :: m
 
  }
  deriving stock (Generic)

{--
instance Has (Settings m) (Application m) where
  getter = config
  modifier f a = a {config = f . config $ a}
--}
