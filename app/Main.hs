{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import App
import Types
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
 resp <- toAPI getUpdates
 case resp of
  Left ErrorBot{..} -> putStrLn  err_msg 
  Right Response{..} -> do 
     when (ok == "false") do
      print $ show (fromMaybe 0 error_code) <> fromMaybe "" description 
     case result of 
      Just r -> do 
       mapM_ (\Update{..} ->
        putStrLn  (text message))
        r
    
     
    
     
