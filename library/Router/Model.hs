module Router.Model where

import Router.Prelude


data Request =
  Request !Method !Path !Query ![Header] !InputStream

data Response =
  Response !Status ![Header] !OutputStream

-- |
-- HTTP Method in lower-case.
newtype Method =
  Method ByteString

newtype Path =
  Path ByteString

data Query =
  Query !ByteString

-- |
-- Header key and value all in lower-case.
data Header =
  Header !ByteString !ByteString

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
