module Utils where

import Control.Applicative

doTimes :: Applicative m => Int -> m a -> m [a]
doTimes 0 ma = pure []
doTimes n ma = liftA2 (:) ma (doTimes (n - 1) ma)

infixr 9 .:
(.:) = (.).(.)
