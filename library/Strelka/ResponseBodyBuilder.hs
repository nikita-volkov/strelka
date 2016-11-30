module Strelka.ResponseBodyBuilder where

import Strelka.Prelude
import qualified Data.ByteString as C
import qualified Data.ByteString.Lazy as D
import qualified Data.ByteString.Builder as E
import qualified Data.Text.Encoding as H
import qualified Data.Text.Lazy as F
import qualified Data.Text.Lazy.Encoding as I
import qualified Data.Text.Lazy.Builder as J


newtype ResponseBodyBuilder =
  ResponseBodyBuilder ((ByteString -> IO ()) -> IO () -> IO ())

instance IsString ResponseBodyBuilder where
  fromString string =
    bytesBuilder (E.stringUtf8 string)

instance Monoid ResponseBodyBuilder where
  mempty =
    ResponseBodyBuilder (\_ flush -> flush)
  mappend (ResponseBodyBuilder cont1) (ResponseBodyBuilder cont2) =
    ResponseBodyBuilder (\consume flush -> cont1 consume (pure ()) *> cont2 consume flush)

instance Semigroup ResponseBodyBuilder


bytes :: ByteString -> ResponseBodyBuilder
bytes x =
  ResponseBodyBuilder (\consume flush -> consume x *> flush)

lazyBytes :: D.ByteString -> ResponseBodyBuilder
lazyBytes x =
  ResponseBodyBuilder (\consume flush -> D.foldlChunks (\io chunk -> io >> consume chunk) (pure ()) x >> flush)

bytesBuilder :: E.Builder -> ResponseBodyBuilder
bytesBuilder =
  lazyBytes . E.toLazyByteString

text :: Text -> ResponseBodyBuilder
text =
  bytesBuilder . H.encodeUtf8Builder

lazyText :: F.Text -> ResponseBodyBuilder
lazyText =
  bytesBuilder . I.encodeUtf8Builder

textBuilder :: J.Builder -> ResponseBodyBuilder
textBuilder =
  lazyText . J.toLazyText

