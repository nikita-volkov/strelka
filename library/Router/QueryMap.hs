module Router.QueryMap where

import Router.Prelude hiding (Header)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap.Strict


-- |
-- Lookup-optimized URL query params.
newtype QueryMap =
  QueryMap (HashMap Text (Maybe Text))
  deriving (Semigroup, Monoid)

fromQuery :: Query -> QueryMap
fromQuery query =
  QueryMap (HashMap.Strict.fromList (queryToQueryText query))

lookup :: Text -> QueryMap -> Maybe (Maybe Text)
lookup name (QueryMap hashMap) =
  HashMap.Strict.lookup name hashMap
