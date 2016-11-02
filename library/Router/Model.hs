module Router.Model where

import Router.Prelude


data Request =
  Request !Method !Path ![Param] ![Header] !InputStream

data Response =
  Response !Status ![Header] !OutputStream

-- |
-- HTTP Method in lower-case.
newtype Method =
  Method ByteString

newtype Path =
  Path ByteString

data Param =
  Param !ByteString !ByteString

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


