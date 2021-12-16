module Error where

import           Control.Error               (ExceptT)
import           Control.Error.Util          (hoistEither)
import           Control.Exception.Base      ()
import           Network.HTTP.Client.Conduit (HttpException (..),
                                              HttpExceptionContent (..))
import           Network.HTTP.Simple         (getResponseStatus)
import Control.Monad.Cont (liftIO, lift)
import Logger.Class




data APIError =HTTPError HttpException |  ParserError String | ConfigError IOError | ConfigErrorJson String
newtype BotError = BotError String

checkError :: Monad m => APIError -> ExceptT BotError m a
checkError (HTTPError (HttpExceptionRequest _ (ConnectionFailure e))) =
  hoistEither $ Left $ BotError $ "connection failure" <> " " <> show e
checkError (HTTPError (HttpExceptionRequest _ (StatusCodeException resp _))) =
  hoistEither $ Left $ BotError $ " no 200: " <> show (getResponseStatus resp)
checkError (HTTPError (InvalidUrlException url er)) =
  hoistEither $ Left $ BotError $ show $ "url : " <> url <> " " <> er
checkError (HTTPError (HttpExceptionRequest _ content)) = hoistEither $ Left $ BotError $ show content
checkError (ParserError err') = hoistEither $ Left $ BotError $ "can not parse: " <> err'
checkError (ConfigError err') = hoistEither $ Left $ BotError $ "can not parse: " <> show err'
checkError (ConfigErrorJson err') = hoistEither $ Left $ BotError err'
