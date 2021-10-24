module Error where

import Network.HTTP.Client.Conduit (HttpException)



data APIError =HTTPError HttpException |  ParserError String
data BotError = BotError String 

