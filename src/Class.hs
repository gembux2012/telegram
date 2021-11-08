{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Class where

import Types
import Control.Monad.Reader (ReaderT, Reader, asks, ask, lift)
import Data.Has (Has, getter)

newtype Answer  = Answer
  { 
    prepareAnswer :: Message -> SendMessage
    
  }

class Monad m  => Answerable m where
  getAnswer :: Message -> SendMessage

  


