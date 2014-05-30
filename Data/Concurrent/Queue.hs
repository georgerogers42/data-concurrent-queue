{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.Concurrent.Queue (TakeQueue(..), PutQueue(..), Take, Put, TakePut, DMVar, newDMVar, newEmptyDMVar) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative

class TakeQueue q m | q -> m where
  take :: q a -> m a

class PutQueue q m | q -> m where
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


newtype Take = Take Take
newtype Put = Put Put
newtype TakePut = TakePut TakePut

newtype DMVar d a = DMVar (MVar a)

newDMVar :: a -> IO (DMVar d a)
newDMVar a = DMVar <$> newMVar a

newEmptyDMVar :: IO (DMVar d a)
newEmptyDMVar = DMVar <$> newEmptyMVar

takeDMVar :: DMVar d a -> IO a
takeDMVar (DMVar x) = takeMVar x

putDMVar :: DMVar d a -> a -> IO ()
putDMVar (DMVar x) a = putMVar x a


instance TakeQueue (DMVar TakePut) IO where
  take = takeDMVar
instance PutQueue (DMVar TakePut) IO where
  put = putDMVar

instance TakeQueue (DMVar Take) IO where
  take = takeDMVar

instance PutQueue (DMVar Put) IO where
  put = putDMVar
