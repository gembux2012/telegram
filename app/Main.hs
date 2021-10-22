{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import App
import Types
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Error (runExceptT, ExceptT)
import Error
import GHC.Show (ShowS)

main :: IO ()

--runBot :: ExceptT e m a -> IO()
runBot api = do
 res <- runExceptT  api
 case res of
  Left  (ParserError err)   -> putStrLn err
  Left (OtherHttpError er) -> putStrLn$ show er 
  Right _ -> putStrLn "Ok"
 return()


main = runBot $ do
  (Response  res) <- toAPI $ GetUpdates Nothing Nothing Nothing Nothing
  liftIO $ putStrLn $ show res
         
    
     
    
     
