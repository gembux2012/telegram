module Error where

import Network.HTTP.Client.Conduit (HttpException)



data APIError =HTTPError HttpException | OtherHttpError String| ParserError String

