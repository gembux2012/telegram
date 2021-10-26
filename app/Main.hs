{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import App
import Types
import Control.Monad (when)
--import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Error (runExceptT, ExceptT,)
import Error
import GHC.Show (ShowS)
import Data.Has (Has, getter, modifier)
import GHC.Generics (Generic)
import Control.Monad.Reader (ReaderT, ask, runReaderT, liftIO, MonadIO, lift)

main :: IO ()

-- runBot :: ExceptT BotError (ReaderT (Application m) IO) a -> IO ()
runBot  api = do
 let app  =
         Application {
            config = return $ Config "https://api.telegram.org/bot2012575953:AAHVSAkJou2YKziQWhmny3K9g32jSRImNt4/" 
           }
 
 res <- runReaderT(runExceptT api) app 
 case res of
  Left  (BotError err)   -> putStrLn err
  Right _ -> putStrLn "bot stopped ok"
 return()

newtype Application m
  = Application {config :: m Config}
  deriving stock Generic 
  
instance Has (m Config ) (Application  m) where
  getter = config
  modifier f a = a {config = f . config $ a}  

--newtype Config = Config String

main =  
 runBot $ do
  (Response'  res) <- toAPI  $ GetUpdates Nothing Nothing Nothing Nothing
  liftIO $ print res
         
    
     
    
     
