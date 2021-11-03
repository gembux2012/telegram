{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Class where

import Types
import Control.Monad.Reader (ReaderT, Reader, asks, ask)
import Data.Has (Has, getter)

newtype Answer  = Answer
  { 
    prepareAnswer :: Message -> SendMessage
    
  }

class Monad m  => Answerable m where
  getAnswer :: Message -> SendMessage

instance
  ( Has (Answer ) r
  ) =>
  Answerable (Reader r )
  where
  getAnswer  mes = ask getter >>= \(Answer doGAFS) -> lift $ doGAFS  id text btn
  


