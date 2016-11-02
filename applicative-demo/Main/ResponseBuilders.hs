module Main.ResponseBuilders where

import Rebase.Prelude
import Router.ResponseBuilder


notFoundInHTML :: ResponseBuilder
notFoundInHTML =
  okayStatus <> contentTypeOfHTML <> body bodyStream
  where
    bodyStream =
      undefined
