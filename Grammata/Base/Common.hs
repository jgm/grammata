module Grammata.Base.Common (rep, todayS) where

import Grammata.Types
import Control.Monad.RWS
import Data.Time

rep :: (Monad m, Monoid a) => Int -> Doc m a -> Doc m a
rep n x = mconcat <$> replicateM n x

todayS :: Doc IO String
todayS = liftIO (show . utctDay <$> getCurrentTime)

