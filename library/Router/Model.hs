module Router.Model where

import Router.Prelude


data Request =
  Request !Method !Path !Query !(HashMap HeaderName HeaderValue) !RequestBody

data Response =
  Response !Status ![Header] !ResponseBody

-- |
-- HTTP Method in lower-case.
newtype Method =
  Method ByteString
  deriving (IsString, Show, Eq, Ord, Hashable)

newtype Path =
  Path ByteString
  deriving (IsString, Show, Eq, Ord, Hashable)

newtype Query =
  Query ByteString
  deriving (IsString, Show, Eq, Ord, Hashable)

data Header =
  Header !HeaderName !HeaderValue

-- |
-- Header name in lower-case.
newtype HeaderName =
  HeaderName ByteString
  deriving (IsString, Show, Eq, Ord, Hashable)

newtype HeaderValue =
  HeaderValue ByteString
  deriving (IsString, Show, Eq, Ord, Hashable)

newtype Status =
  Status Int

-- |
-- IO action, which produces the next chunk.
-- An empty chunk signals the end of the stream.
newtype RequestBody =
  RequestBody (IO ByteString)

-- |
-- A function on a chunk consuming and flushing IO actions.
newtype ResponseBody =
  ResponseBody ((ByteString -> IO ()) -> IO () -> IO ())

instance IsString ResponseBody where
  fromString string =
    ResponseBody (\sendChunk flush -> sendChunk (fromString string) >> flush)
