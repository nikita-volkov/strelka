module Router.ResponseBuilder where

import Router.Prelude
import Router.Model


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
  ResponseBuilder (\(Response status headers body) -> Response status (Header name value : headers) body)

status :: Int -> ResponseBuilder
status x =
  ResponseBuilder (\(Response _ headers body) -> Response (Status x) headers body)

body :: OutputStream -> ResponseBuilder
body x =
  ResponseBuilder (\(Response status headers _) -> Response status headers x) 


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
