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

list :: [(ByteString, ByteString)] -> HeadersMap
list list =
  HeadersMap (HashMap.Strict.fromList (map (mapFirst lowerCaseBytes_iso_8859_1) list))
  where
    mapFirst f (a, b) =
      (f a, b)

folder :: (forall b. (b -> (ByteString, ByteString) -> b) -> b -> b) -> HeadersMap
folder fold =
  HeadersMap (fold (\(!map) (!k, !v) -> HashMap.Strict.insert (lowerCaseBytes_iso_8859_1 k) v map) HashMap.Strict.empty)

folderM :: Functor m => (forall b. (b -> (ByteString, ByteString) -> b) -> b -> m b) -> m HeadersMap
folderM fold =
  fmap HeadersMap (fold (\(!map) (!k, !v) -> HashMap.Strict.insert (lowerCaseBytes_iso_8859_1 k) v map) HashMap.Strict.empty)

lookup :: ByteString -> HeadersMap -> Maybe ByteString
lookup name (HeadersMap hashMap) =
  HashMap.Strict.lookup (lowerCaseBytes_iso_8859_1 name) hashMap

-- |
-- Lowercase according to ISO-8859-1.
lowerCaseBytes_iso_8859_1 :: ByteString -> ByteString
lowerCaseBytes_iso_8859_1 =
  ByteString.map byteTransformation
  where
    byteTransformation w =
      if isTransformable w
        then w + 32
        else w
      where
        isTransformable w =
          65 <= w && w <=  90 ||
          192 <= w && w <= 214 ||
          216 <= w && w <= 222
