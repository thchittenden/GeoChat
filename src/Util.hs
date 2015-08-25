module Util where

import Data.Map as Map
import Servant
import Control.Monad.Trans.Either

type Servlet a = EitherT ServantErr IO a

lookupOrInsertM :: (Ord k, Monad m) => k -> m v -> Map.Map k v -> m (v, Map.Map k v)
lookupOrInsertM k mv m =
    case Map.lookup k m of
        Just v -> return (v, m)
        Nothing -> do
            v <- mv
            return (v, Map.insert k v m)
