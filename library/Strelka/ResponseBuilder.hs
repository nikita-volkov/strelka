module Strelka.ResponseBuilder where

import Strelka.Prelude
import Strelka.Model
import qualified Strelka.ResponseBodyBuilder as A


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

body :: A.ResponseBodyBuilder -> ResponseBuilder
body (A.ResponseBodyBuilder x) =
  ResponseBuilder (\(Response status headers _) -> Response status headers (OutputStream x)) 


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

text :: A.ResponseBodyBuilder -> ResponseBuilder
text x =
  contentTypeOfText <> body x

html :: A.ResponseBodyBuilder -> ResponseBuilder
html x =
  contentTypeOfHTML <> body x

json :: A.ResponseBodyBuilder -> ResponseBuilder
json x =
  contentTypeOfJSON <> body x

unauthorized :: ByteString -> ResponseBuilder
unauthorized realm =
  unauthorizedStatus <> header "WWW-Authenticate" ("Basic realm=\"" <> realm <> "\"")
