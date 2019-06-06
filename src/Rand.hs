module Rand where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import System.Random.MWC

newtype Rand a = Rand (ReaderT (GenIO) (ST RealWorld) a)
  deriving (Functor, Applicative, Monad)

runRandIOGen :: Rand a -> GenIO -> IO a
runRandIOGen (Rand ma) rand = stToIO (runReaderT ma rand)

runRandIO :: Rand a -> IO a
runRandIO ma = runRandIOGen ma =<< createSystemRandom

getRand :: Variate a => Rand a
getRand = Rand $ do
  gen <- ask
  lift $ uniform gen
