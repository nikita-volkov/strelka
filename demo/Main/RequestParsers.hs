module Main.RequestParsers where

import Rebase.Prelude
import Router.RequestParser
import Router.ParamsParser
import Router.ResponseBuilder (ResponseBuilder)
import Main.Effect (Effect)
import qualified Main.Effect as B
import qualified Main.ResponseBuilders as A
import qualified Router.ResponseBuilder as C


type Route =
  RequestParser B.Effect ResponseBuilder

top :: Route
top =
  consumeSegmentIfIs "numbers" *> numbers <|>
  consumeSegmentIfIs "users" *> users <|>
  notFound
  where
    numbers =
      ensureThatMethodIsGet *> get <|>
      ensureThatMethodIsPut *> put
      where
        get =
          ensureThatAcceptsJSON *> json <|>
          ensureThatAcceptsHTML *> html
          where
            json =
              A.listNumbersAsJSON <$> lift B.listNumbers
            html =
              A.listNumbersAsHTML <$> lift B.listNumbers
        put =
          authorizing "" authorized
          where
            authorized =
              do
                bytes <- consumeBodyAsStrictBytes
                undefined
    users =
      undefined
    notFound =
      ensureThatAcceptsHTML *> html <|>
      text
      where
        html =
          pure A.notFoundInHTML
        text =
          pure A.notFoundInText

-- |
-- Reusable route for wrapping other routes with HTTP Authorization.
authorizing :: ByteString -> Route -> Route
authorizing realm authorized =
  authorize *> authorized <|>
  unauthorized
  where
    authorize =
      do
        (username, password) <- getAuthorization
        success <- lift (B.authorize username password)
        guard success
    unauthorized =
      pure (C.unauthorized realm)
