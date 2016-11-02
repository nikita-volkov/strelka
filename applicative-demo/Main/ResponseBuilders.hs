module Main.ResponseBuilders where

import Rebase.Prelude
import Router.ResponseBuilder


notFoundInHTML :: ResponseBuilder
notFoundInHTML =
  okayStatus <> contentTypeOfHTML <> body bodyStream
  where
    bodyStream =
      "<h1>404 Not Found</h1>"

notFoundInText :: ResponseBuilder
notFoundInText =
  okayStatus <> contentTypeOfText <> body bodyStream
  where
    bodyStream =
      "404 Not Found"

getPassword :: Text -> ResponseBuilder
getPassword username =
  undefined
