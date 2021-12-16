{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Telegram.Api where

import Control.Monad.Reader (ReaderT)

import Error (BotError)

import Control.Error (ExceptT)
import  qualified Data.ByteString.Char8 as BS8
import Logger.Class (Logger, logI)
import Telegram.Types (TelegramOpts(..), Updates(..),Update'(..), GetUpdates(..))
import Control.Monad.State.Lazy (StateT)
import qualified Data.Map as Map
import Control.Monad.RWS.Class (asks, get, modify)
import Data.Has (getter)
import Telegram.Class (toAPI)
import Control.Monad.Cont (unless)
import Telegram.Types --(Url(..), TelegramOpts(..), GetUpdates(..), Updates(..), SendMessage(..), Update(..),Message(..), From(..),)
import App (prepareAnswer)
telegram :: ExceptT BotError (ReaderT (Logger IO, TelegramOpts) (StateT (Map.Map Integer Integer) IO)) ()
telegram  = do
  logI "bot launched"
  getUpdates (Just 20) Nothing (Just 20) Nothing

getUpdates :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe [String] -> ExceptT BotError (ReaderT (Logger IO, TelegramOpts) (StateT (Map.Map Integer Integer) IO)) ()
getUpdates  o l t a_u  = do
      TelegramOpts _ _ btn  <- asks getter
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


