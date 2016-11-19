module Router.ResponseBody where

import Router.Prelude
import qualified Data.ByteString as C
import qualified Data.ByteString.Lazy as D
import qualified Data.ByteString.Builder as E
import qualified Data.Text.Encoding as H
import qualified Data.Text.Lazy as F
import qualified Data.Text.Lazy.Encoding as I
import qualified Data.Text.Lazy.Builder as J


-- |
-- A function on the chunk consuming and flushing IO actions.
newtype ResponseBody =
  ResponseBody ((ByteString -> IO ()) -> IO () -> IO ())

instance IsString ResponseBody where
  fromString string =
    bytesBuilder (E.stringUtf8 string)

bytes :: ByteString -> ResponseBody
bytes x =
  ResponseBody (\consume flush -> consume x *> flush)

lazyBytes :: D.ByteString -> ResponseBody
lazyBytes x =
  ResponseBody (\consume flush -> D.foldlChunks (\io chunk -> io >> consume chunk) (pure ()) x >> flush)

bytesBuilder :: E.Builder -> ResponseBody
bytesBuilder =
  lazyBytes . E.toLazyByteString

text :: Text -> ResponseBody
text =
  bytesBuilder . H.encodeUtf8Builder

lazyText :: F.Text -> ResponseBody
lazyText =
  bytesBuilder . I.encodeUtf8Builder

textBuilder :: J.Builder -> ResponseBody
textBuilder =
  lazyText . J.toLazyText

