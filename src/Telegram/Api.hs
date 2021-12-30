{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
module Telegram.Api where

import           Control.Monad.Reader     (ReaderT)

import           Error                    (BotError)

import           App                      (prepareAnswer)
import           Control.Error            (ExceptT, fromMaybe)
import           Control.Monad.Cont       (unless)
import           Control.Monad.RWS.Class  (asks, get, modify)
import           Control.Monad.State.Lazy (StateT)
import qualified Data.ByteString.Char8    as BS8
import           Data.Has                 (getter)
import qualified Data.Map                 as Map
import           Logger.Class             (Logger, logI)
import           Class           (toAPI)
import Telegram.Types
import Config.Types (Config(..), )


telegram :: ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
telegram  = do
  config <- asks getter
  case (telegramOpts config)  of
   Just opts -> do  
      logI "bot telegram launched"
      let route = host opts <> path opts
      getUpdates (Just 20) Nothing (Just 20) Nothing
      
 where
  --getUpdates :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe [String] -> ExceptT BotError (ReaderT (Logger IO, Config) (StateT (Map.Map Integer Integer) IO)) ()
  getUpdates  o l t a_u  = do
        (Updates update) <- toAPI route $ GetUpdates o l t a_u
        logI "awaiting message"
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
  

