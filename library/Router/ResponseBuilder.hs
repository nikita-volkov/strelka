module Router.ResponseBuilder where

import Router.Prelude
import Router.Model
import qualified Data.ByteString as C
import qualified Data.ByteString.Lazy as D
import qualified Data.ByteString.Builder as E
import qualified Data.Text.Encoding as H
import qualified Data.Text.Lazy as F
import qualified Data.Text.Lazy.Encoding as I
import qualified Data.Text.Lazy.Builder as J


newtype ResponseBuilder =
  ResponseBuilder (Response -> Response)

instance Monoid ResponseBuilder where
  mempty =
    ResponseBuilder id
  mappend (ResponseBuilder fn1) (ResponseBuilder fn2) =
    ResponseBuilder (fn2 . fn1)

instance Semigroup ResponseBuilder


run :: ResponseBuilder -> Response
run (ResponseBuilder fn) =
  fn (Response (Status 200) [] (OutputStream (const (const (pure ())))))

header :: ByteString -> ByteString -> ResponseBuilder
header name value =
  ResponseBuilder (\(Response status headers body) -> Response status (Header (HeaderName name) (HeaderValue value) : headers) body)

status :: Int -> ResponseBuilder
status x =
  ResponseBuilder (\(Response _ headers body) -> Response (Status x) headers body)

body :: OutputStream -> ResponseBuilder
body x =
  ResponseBuilder (\(Response status headers _) -> Response status headers x) 

bodyFromBytes :: ByteString -> ResponseBuilder
bodyFromBytes x =
  body (OutputStream (\consume flush -> consume x *> flush))

bodyFromLazyBytes :: D.ByteString -> ResponseBuilder
bodyFromLazyBytes x =
  body (OutputStream (\consume flush -> D.foldlChunks (\io chunk -> io >> consume chunk) (pure ()) x >> flush))

bodyFromBytesBuilder :: E.Builder -> ResponseBuilder
bodyFromBytesBuilder =
  bodyFromLazyBytes . E.toLazyByteString

bodyFromText :: Text -> ResponseBuilder
bodyFromText =
  bodyFromBytesBuilder . H.encodeUtf8Builder

bodyFromLazyText :: F.Text -> ResponseBuilder
bodyFromLazyText =
  bodyFromBytesBuilder . I.encodeUtf8Builder

bodyFromTextBuilder :: J.Builder -> ResponseBuilder
bodyFromTextBuilder =
  bodyFromLazyText . J.toLazyText


-- * Predefined composites
-------------------------

contentType :: ByteString -> ResponseBuilder
contentType x =
  header "content-type" x

contentTypeOfText :: ResponseBuilder
contentTypeOfText =
  contentType "text/plain"

contentTypeOfHTML :: ResponseBuilder
contentTypeOfHTML =
  contentType "text/html"

contentTypeOfJSON :: ResponseBuilder
contentTypeOfJSON =
  contentType "application/json"

okayStatus :: ResponseBuilder
okayStatus =
  status 200

notFoundStatus :: ResponseBuilder
notFoundStatus =
  status 404

unauthorizedStatus :: ResponseBuilder
unauthorizedStatus =
  status 401

badRequestStatus :: ResponseBuilder
badRequestStatus =
  status 400

internalErrorStatus :: ResponseBuilder
internalErrorStatus =
  status 500

text :: OutputStream -> ResponseBuilder
text x =
  contentTypeOfText <> body x

html :: OutputStream -> ResponseBuilder
html x =
  contentTypeOfHTML <> body x

json :: OutputStream -> ResponseBuilder
json x =
  contentTypeOfJSON <> body x

unauthorized :: ByteString -> ResponseBuilder
unauthorized realm =
  unauthorizedStatus <> header "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\"")
