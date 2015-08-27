module Util where

import Data.Map as Map
import Servant
import Control.Monad.Trans.Either
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text
import Data.Text.Encoding
import Text.HTML.SanitizeXSS
import Debug.Trace

type Servlet a = EitherT ServantErr IO a

lookupOrInsertM :: (Ord k, Monad m) => k -> m v -> Map.Map k v -> m (v, Map.Map k v)
lookupOrInsertM k mv m =
    case Map.lookup k m of
        Just v -> return (v, m)
        Nothing -> do
            v <- mv
            return (v, Map.insert k v m)

sanitizeString :: String -> String
sanitizeString = unpack . sanitize . pack

sanitizeByteString :: BS.ByteString -> BS.ByteString
sanitizeByteString arg = trace ("BS: " ++ show arg) (encodeUtf8 . sanitize . decodeUtf8 $ arg)

sanitizeByteString' :: LBS.ByteString -> LBS.ByteString
sanitizeByteString' arg = trace ("LBS: " ++ show arg) (LBS.fromStrict . sanitizeByteString . LBS.toStrict $ arg)
