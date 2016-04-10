module Router.RequestProperties
(
  Properties,
  fromRequest,
  request,
  method,
  lookupHeader,
  lookupParam,
)
where

import Router.Prelude hiding (Header)
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashMap.Strict as HashMap.Strict
import qualified Router.HeadersMap as HeadersMap
import qualified Router.QueryMap as QueryMap
import qualified Network.Wai as Wai


-- |
-- Lookup and analysis-optimized representation of the request properties.
data Properties =
  Properties {
    request :: !Wai.Request,
    method :: !Method,
    headersMap :: !HeadersMap.HeadersMap,
    queryMap :: !QueryMap.QueryMap
  }

fromRequest :: Request -> Properties
fromRequest request =
  Properties request method headersMap queryMap
  where
    method =
      Wai.requestMethod request
    headersMap =
      HeadersMap.fromList $
      map (mapFirst (CaseInsensitive.original)) $
      Wai.requestHeaders request
      where
        mapFirst f (a, b) =
          (f a, b)
    queryMap =
      QueryMap.fromQuery $
      Wai.queryString request

lookupHeader :: ByteString -> Properties -> Maybe ByteString
lookupHeader name =
  HeadersMap.lookup name . headersMap

lookupParam :: Text -> Properties -> Maybe (Maybe Text)
lookupParam name =
  QueryMap.lookup name . queryMap
