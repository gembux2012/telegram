{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import App
import Types --(Response'(..), Update(..), Config(..), GetUpdates(..), SendMessage(..), GetUpdates, mesText, From(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Error (runExceptT, ExceptT,)
import Error
import GHC.Show (ShowS)
import Data.Has (Has, getter, modifier)
import GHC.Generics (Generic)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO, MonadIO, lift, unless)

main :: IO ()

--runBot :: (Monad m, MonadIO IO) => ExceptT BotError (ReaderT (Application m) m) a -> IO ()
runBot  api = do
 res <- runReaderT(runExceptT api) app 
 case res of
  Left  (BotError err)   -> putStrLn err
  Right _ -> putStrLn "bot stopped ok"
 return()
 where 
  app :: Application IO
  app  =
     Application {
        config = Settings (pure (Config "https://api.telegram.org/bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/"))
         
       }
 

newtype Application m
  = Application {config :: Settings m}
  deriving stock Generic 
  
instance Has (Settings m ) (Application  m) where
  getter = config
  modifier f a = a {config = f . config $ a}  



main = runBot telegram 
         
telegram =  
  getUpdates Nothing Nothing  (Just 20) Nothing
  
getUpdates o l t a_u = do 
    liftIO $ putStrLn "waiting for an answer"
    (Response' res) <- toAPI $ GetUpdates o l t a_u
    unless (null res) do 
      mapM_
         (\Update {..}
            -> do 
            liftIO $ putStrLn "sendmessage"
            _ <- toAPI $ SendMessage (fromId (mesFrom message)) (mesText message)
            getUpdates Nothing Nothing (Just 20) Nothing
                  )
          res 
    getUpdates Nothing Nothing (Just 20) Nothing
 
    
     
