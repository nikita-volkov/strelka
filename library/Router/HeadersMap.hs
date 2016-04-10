module Router.HeadersMap where

import Router.Prelude hiding (Header)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap.Strict


-- |
-- Lookup-optimized headers.
newtype HeadersMap =
  HeadersMap (HashMap ByteString ByteString)
  deriving (Semigroup, Monoid)

fromList :: [(ByteString, ByteString)] -> HeadersMap
fromList list =
  HeadersMap (HashMap.Strict.fromList (map (mapFirst lowerCaseBytes_iso_8859_1) list))
  where
    mapFirst f (a, b) =
      (f a, b)

fromFolder :: (forall b. (b -> (ByteString, ByteString) -> b) -> b -> b) -> HeadersMap
fromFolder fold =
  HeadersMap (fold (\(!map) (!k, !v) -> HashMap.Strict.insert (lowerCaseBytes_iso_8859_1 k) v map) HashMap.Strict.empty)

fromFolderM :: Functor m => (forall b. (b -> (ByteString, ByteString) -> b) -> b -> m b) -> m HeadersMap
fromFolderM fold =
  fmap HeadersMap (fold (\(!map) (!k, !v) -> HashMap.Strict.insert (lowerCaseBytes_iso_8859_1 k) v map) HashMap.Strict.empty)

lookup :: ByteString -> HeadersMap -> Maybe ByteString
lookup name (HeadersMap hashMap) =
  HashMap.Strict.lookup (lowerCaseBytes_iso_8859_1 name) hashMap
