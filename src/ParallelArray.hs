module ParallelArray where

import Control.Concurrent hiding (forkIO)
import Control.Concurrent.Thread
import Control.Exception.Base
import Control.Monad.Except
import Data.Array

data ThreadException = ThreadThrew ThreadId SomeException
  deriving Show

parallelMkArray :: forall m a i. (MonadError ThreadException m, MonadIO m, Ix i)
  => (i -> IO a) -> (i, i) -> m (Array i a)
parallelMkArray thread bounds = do
  let f ix = thread ix
      indices = (range bounds)
  threads <- liftIO $ traverse (forkIO . f) indices
  results <- traverse getResult threads
  pure (listArray bounds results)
  where
    getResult (tId, ma) = liftIO ma >>= \case
      Left err -> throwError (ThreadThrew tId err)
      Right r -> pure r
