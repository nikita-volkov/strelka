module Router.Model where

import Router.Prelude


data Request =
  Request !Method !Path !Query !(HashMap HeaderName HeaderValue) !InputStream

data Response =
  Response !Status ![Header] !OutputStream

-- |
-- HTTP Method in lower-case.
newtype Method =
  Method ByteString

newtype Path =
  Path ByteString

newtype Query =
  Query ByteString

data Header =
  Header !HeaderName !HeaderValue

-- |
-- Header name in lower-case.
newtype HeaderName =
  HeaderName ByteString

newtype HeaderValue =
  HeaderValue ByteString

newtype Status =
  Status Int

-- |
-- IO action, which produces the next chunk.
-- An empty chunk signals the end of the stream.
newtype InputStream =
  InputStream (IO ByteString)

-- |
-- A function on a chunk consuming and flushing IO actions.
newtype OutputStream =
  OutputStream ((ByteString -> IO ()) -> IO () -> IO ())

instance IsString OutputStream where
  fromString string =
    OutputStream (\sendChunk flush -> sendChunk (fromString string) >> flush)
