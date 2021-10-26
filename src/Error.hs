module Error where

import Network.HTTP.Client.Conduit (HttpException(..),HttpExceptionContent(..), )
import Types
import Control.Error.Util (hoistEither)
import Network.HTTP.Simple (getResponseStatus)



data APIError =HTTPError HttpException |  ParserError String
data BotError = BotError String 

checkError (HTTPError(HttpExceptionRequest _  (ConnectionFailure e))) = do
            hoistEither $ Left $ BotError "connection failure"
checkError (HTTPError(HttpExceptionRequest _  (StatusCodeException resp  _))) = do
            hoistEither $ Left $ BotError $ " no 200 " <> show (getResponseStatus resp)
checkError (HTTPError(InvalidUrlException url er)) = do
            hoistEither $ Left $ BotError $ show $ "url: " <> url <> " " <> er         
checkError (HTTPError(HttpExceptionRequest _ content)) = do
             hoistEither $ Left $ BotError $ show content    
checkError (ParserError err') = do 
             hoistEither $ Left $ BotError $ "can not parse: " <>  err' 