{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.Concurrent.Queue (TakeQueue(..), PutQueue(..)) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative

class TakeQueue q m where
  take :: q a -> m a

class PutQueue q m  where
  put :: q a -> a -> m ()

instance TakeQueue MVar IO where
  take = takeMVar
instance PutQueue MVar IO where
  put = putMVar

instance TakeQueue Chan IO where
  take = readChan
instance PutQueue Chan IO where
  put = writeChan

instance TakeQueue TMVar STM where
  take = takeTMVar
instance PutQueue TMVar STM where
  put = putTMVar


instance TakeQueue TChan STM where
  take = readTChan
instance PutQueue TChan STM where
  put = writeTChan
